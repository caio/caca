use gix::{
    bstr::{BStr, ByteSlice},
    object::tree::diff::{change::Event, Action},
    ObjectId, Tree,
};

use std::path::{Path, PathBuf};

use crate::error::{wrap_err, WrappedError};

pub(crate) enum RenameError<E> {
    Repo(E),
    Wrapped(WrappedError),
}

impl<E> From<WrappedError> for RenameError<E> {
    fn from(value: WrappedError) -> Self {
        RenameError::Wrapped(value)
    }
}

// Find out if `path` was created via renaming a path
// from the `parent` tree
//
// Takes a path that exists in the current tree and does
// NOT exist in the parent
//
// Yields back the path and object id of a blob in the
// parent tree that is most similar to the one pointed
// at by the input path
pub(crate) fn find_rename<R, E, P>(
    repo: R,
    _path: P,
    // the path at the current tree points at the blob id
    id: ObjectId,
    current: &Tree<'_>,
    parent: &Tree<'_>,
) -> Result<Option<(PathBuf, ObjectId)>, RenameError<E>>
where
    R: Repo<Error = E>,
    P: AsRef<Path>,
{
    #[cfg(debug)]
    {
        let mut buf = Vec::new();
        assert!(
            current
                .lookup_entry_by_path(&_path, &mut buf)
                .expect("entry lookup via path works")
                .is_some(),
            "precondition failure: given path {:?} does not exist in current tree {}",
            _path.as_ref(),
            current.id
        );

        assert!(
            parent
                .lookup_entry_by_path(&_path, &mut buf)
                .expect("entry lookup via path works")
                .is_none(),
            "precondition failure: given path {:?} exists in parent tree {}",
            _path.as_ref(),
            parent.id
        );
    }

    let mut candidates = Vec::new();
    let target_header = repo.get_header(id).map_err(RenameError::Repo)?;

    // stop conditions:
    // - error
    // - identity found
    let mut map_err = None;
    let mut by_identity = None;

    map_deleted_blobs(current, parent, |location, id| -> bool {
        if target_header.id == id {
            // trivial rename: path changed, blob content didn't
            by_identity = Some(gix::path::from_bstr(location).into_owned());
            return false;
        }
        match repo.get_header(id) {
            Ok(header) => {
                // pruning candidates set:
                // since similarity is based on the byte length of the
                // diff, the total sizes can be used to figure out
                // if there's even a chance they'll be similar enough
                let max = target_header.size.max(header.size) as f32;
                let delta = target_header.size.abs_diff(header.size) as f32;
                if max * repo.min_similarity() >= delta {
                    // XXX should sniff the bytes prior to mime-based filtering
                    let path = location.to_path_lossy();
                    let (guessed, is_text) =
                        crate::mime::guess_from_path(&path).unwrap_or((crate::mime::BINARY, false));
                    if is_text {
                        candidates.push((path.into_owned(), header));
                    } else {
                        tracing::trace!(
                            path = tracing::field::debug(location),
                            mime = guessed,
                            "skipped: not text"
                        );
                    }
                } else {
                    tracing::trace!(
                        path = tracing::field::debug(location),
                        "skipped: size difference too large"
                    );
                }
                true
            }
            Err(e) => {
                map_err = Some(e);
                false
            }
        }
    })?;

    if let Some(err) = map_err {
        return Err(RenameError::Repo(err));
    }

    if let Some(found) = by_identity {
        return Ok(Some((found, id)));
    }

    if candidates.is_empty() {
        return Ok(None);
    }

    // The set of candidates is potentially massive,
    // checking the similarity of every single one would
    // be too costly so i'll use a heuristic that tries
    // to look at the most likely candidates first and
    // stops as soon as it finds one that's similar enough.
    //
    // Worst case is when a rename can't be found.
    //
    // Since byte delta is the criteria, prioritize
    // blobs with similar length to the target.
    // Break even with the path, in order to guarantee
    // output stability
    candidates.sort_unstable_by(|a, b| {
        a.1.size
            .abs_diff(target_header.size)
            .cmp(&b.1.size.abs_diff(target_header.size))
            .then_with(|| a.1.id.cmp(&b.1.id))
    });
    tracing::trace!(
        candidates = tracing::field::debug(&candidates),
        "rename candidates"
    );

    for (path, header) in candidates {
        let similarity = repo.similarity(id, header.id).map_err(RenameError::Repo)?;
        if similarity >= repo.min_similarity() {
            return Ok(Some((path, header.id)));
        } else {
            tracing::trace!(
                path = tracing::field::debug(&path),
                threshold = repo.min_similarity(),
                similarity,
                "not similar enough"
            );
        }
    }

    Ok(None)
}

pub(crate) trait Repo {
    type Error;

    // FIXME ensure within 0..=1
    fn min_similarity(&self) -> f32;

    fn get_header(&self, id: ObjectId) -> std::result::Result<crate::diff::Header, Self::Error>;

    fn similarity(&self, prev: ObjectId, cur: ObjectId) -> std::result::Result<f32, Self::Error>;
}

// maps every blob that would be removed when transforming
// `parent` into `current`
fn map_deleted_blobs<F>(
    current: &Tree<'_>,
    parent: &Tree<'_>,
    mut cb: F,
) -> Result<(), WrappedError>
where
    F: FnMut(&BStr, ObjectId) -> bool,
{
    let outcome = parent
        .changes()
        .map_err(|e| {
            wrap_err(
                format!("preparing to diff tree {} vs {}", parent.id, current.id,),
                e,
            )
        })?
        .track_path()
        .track_rewrites(None)
        .for_each_to_obtain_tree(current, |change| -> std::result::Result<_, WrappedError> {
            if let Event::Deletion { entry_mode, id } = change.event {
                if entry_mode.is_blob() && !cb(change.location, id.detach()) {
                    return Ok(Action::Cancel);
                }
            };
            Ok(Action::Continue)
        });

    match outcome {
        // If comparing the trees yields no error or gets cancelled
        // manually, everything went fine
        Ok(_)
        | Err(gix::object::tree::diff::for_each::Error::Diff(
            gix::diff::tree::changes::Error::Cancelled,
        )) => Ok(()),
        Err(e) => Err(wrap_err(
            format!(
                "walking diff between trees {} and {}",
                current.id, parent.id
            ),
            e,
        )),
    }
}
