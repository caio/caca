use std::{borrow::Cow, ops::Range};

use gix::{
    bstr::{BStr, ByteSlice},
    diff::Rewrites,
    object::tree::diff::for_each::Error as ForEachError,
    object::tree::diff::{change::Event as GixEvent, Action},
    objs::tree::EntryMode,
    Commit, ObjectId, Tree,
};

mod sink;
pub(crate) use sink::{diff, similarity};

use crate::{
    error::{wrap_err, WrappedError},
    mime::{self, File},
};

pub fn diff_commits<R, E, F>(
    repo: &R,
    base: Commit<'_>,
    parent: Option<Commit<'_>>,
    mut visitor: F,
) -> Result<(), E>
where
    R: Repo<Error = E>,
    E: std::error::Error + Send + Sync + 'static,
    F: FnMut(Event),
{
    let tree = base
        .tree()
        .map_err(|e| wrap_err(format!("reading tree from commit {}", base.id), e))?;
    let parent_tree = {
        if let Some(p) = parent {
            p.tree()
                .map_err(|e| wrap_err(format!("reading tree from commit {}", p.id), e))?
        } else {
            repo.empty_tree()
        }
    };

    let foreach_res = parent_tree
        .changes()
        .map_err(|e| {
            wrap_err(
                format!(
                    "error preparing to diff tree {} vs {}",
                    tree.id, parent_tree.id
                ),
                e,
            )
        })?
        .track_path()
        // FIXME using gix's rename detection here, but using mine on log
        .track_rewrites(Some(Rewrites {
            copies: None,
            percentage: repo.min_similarity(),
            limit: 0,
        }))
        .for_each_to_obtain_tree(&tree, |change| -> Result<Action, E> {
            match change.event {
                GixEvent::Addition { entry_mode, id } => {
                    if let Some(event) =
                        handle_change(&repo, change.location, entry_mode, true, id)?
                    {
                        visitor(Event::Addition(event));
                    }
                }
                GixEvent::Deletion { entry_mode, id } => {
                    if let Some(event) =
                        handle_change(&repo, change.location, entry_mode, false, id)?
                    {
                        visitor(Event::Deletion(event));
                    }
                }
                GixEvent::Modification {
                    previous_entry_mode,
                    previous_id,
                    entry_mode,
                    id,
                } => {
                    // if both sides aren't blobs, handle as if it
                    // were a Deletion followed by an Addition
                    // FIXME can yield a Modification when entry modes are equal eh
                    if !(entry_mode.is_blob() || previous_entry_mode.is_blob()) {
                        if let Some(event) = handle_change(
                            repo,
                            change.location,
                            previous_entry_mode,
                            false,
                            previous_id,
                        )? {
                            visitor(Event::Deletion(event));
                        }
                        if let Some(event) =
                            handle_change(&repo, change.location, entry_mode, true, id)?
                        {
                            visitor(Event::Addition(event));
                        }
                    } else {
                        handle_modification(
                            &repo,
                            change.location,
                            previous_entry_mode,
                            previous_id,
                            entry_mode,
                            id,
                        )
                        .map(|(src_object, change)| {
                            visitor(Event::Modification {
                                src: src_object,
                                change,
                            });
                        })?;
                    }
                }
                GixEvent::Rewrite {
                    source_location,
                    source_entry_mode,
                    source_id,
                    diff: _,
                    entry_mode,
                    id,
                    copy: _,
                } => {
                    handle_modification(
                        &repo,
                        change.location,
                        source_entry_mode,
                        source_id,
                        entry_mode,
                        id,
                    )
                    .map(|(src_object, change)| {
                        visitor(Event::Rename {
                            src: src_object,
                            change,
                            src_path: source_location.to_path_lossy().into_owned(),
                        });
                    })?;
                }
            };

            Ok(Action::Continue)
        });

    match foreach_res {
        Ok(_) => Ok(()),
        Err(ForEachError::ForEach(erased)) => match erased.downcast::<DiffError<E>>() {
            Ok(err) => Err(*err),
            Err(other) => {
                // the foreach api erases the static marker
                // and this branch shouldn't trigger at all so
                // yolo it is.
                Err(wrap_err(format!("{:?}", other), UnknownError))?
            }
        },
        Err(e) => Err(wrap_err(
            format!("changes between {} and {:?}", tree.id, parent_tree.id,),
            e,
        ))?,
    }
}

#[derive(Clone)]
pub struct UnifiedDiff {
    pub before_lineno: usize,
    pub after_lineno: usize,
    pub chunks: Vec<Chunk>,
}

impl std::fmt::Debug for UnifiedDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.chunks {
            c.format_patch(f)?;
        }
        Ok(())
    }
}

impl UnifiedDiff {
    pub fn is_empty(&self) -> bool {
        self.chunks.is_empty()
    }

    pub fn as_patch(&self) -> std::result::Result<String, std::fmt::Error> {
        let mut patch = String::new();
        for chunk in &self.chunks {
            chunk.format_patch(&mut patch)?;
        }
        Ok(patch)
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub before_pos: Range<u32>,
    pub after_pos: Range<u32>,
    pub lines: Vec<Line>,
}

impl Chunk {
    pub fn format_patch<F: std::fmt::Write>(&self, f: &mut F) -> std::fmt::Result {
        writeln!(
            f,
            "@@ -{},{} +{},{} @@",
            self.before_pos.start + 1,
            self.before_pos.len(),
            self.after_pos.start + 1,
            self.after_pos.len(),
        )?;
        for line in &self.lines {
            match line {
                Line::Addition(s) => writeln!(f, "+{}", s)?,
                Line::Removal(s) => writeln!(f, "-{}", s)?,
                Line::Context(s) => writeln!(f, " {}", s)?,
            };
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum Line {
    Addition(String),
    Removal(String),
    Context(String),
}

impl std::fmt::Debug for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Addition(s) => write!(f, "\"+{}\"", s),
            Line::Removal(s) => write!(f, "\"-{}\"", s),
            Line::Context(s) => write!(f, "\" {}\"", s),
        }
    }
}

#[derive(Debug)]
pub enum DiffError<E> {
    Wrapped(WrappedError),
    Repo(E),
}

impl<E> From<WrappedError> for DiffError<E> {
    fn from(value: WrappedError) -> Self {
        DiffError::Wrapped(value)
    }
}

impl<E> std::fmt::Display for DiffError<E>
where
    E: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiffError::Wrapped(w) => w.fmt(f),
            DiffError::Repo(w) => w.fmt(f),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct UnknownError;

impl std::fmt::Display for UnknownError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unknown error")
    }
}

impl std::error::Error for UnknownError {}

impl<E> std::error::Error for DiffError<E> where E: std::error::Error {}

pub type Result<T, E> = std::result::Result<T, DiffError<E>>;

pub trait Repo {
    type Error: std::error::Error + Send + Sync + 'static;

    fn min_similarity(&self) -> Option<f32>;

    fn max_bytes(&self) -> u64;

    fn empty_tree(&self) -> Tree<'_>;

    fn load(&self, id: ObjectId, buf: &mut Vec<u8>) -> std::result::Result<(), Self::Error>;

    fn get_header(&self, id: ObjectId) -> std::result::Result<Header, Self::Error>;

    fn decode_text<'a>(&self, data: &'a [u8]) -> std::result::Result<Cow<'a, str>, Self::Error>;
}

impl<E, T> Repo for &T
where
    T: Repo<Error = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    type Error = E;

    fn max_bytes(&self) -> u64 {
        T::max_bytes(self)
    }

    fn min_similarity(&self) -> Option<f32> {
        T::min_similarity(self)
    }

    fn empty_tree(&self) -> Tree<'_> {
        T::empty_tree(self)
    }

    fn load(&self, id: ObjectId, buf: &mut Vec<u8>) -> std::result::Result<(), Self::Error> {
        T::load(self, id, buf)
    }

    fn get_header(&self, id: ObjectId) -> std::result::Result<Header, Self::Error> {
        T::get_header(self, id)
    }

    fn decode_text<'a>(&self, data: &'a [u8]) -> std::result::Result<Cow<'a, str>, Self::Error> {
        T::decode_text(self, data)
    }
}

fn handle_change<R, E>(
    repo: &R,
    filename: &BStr,
    entry_mode: EntryMode,
    is_addition: bool, // FIXME eurgh
    id: gix::Id<'_>,
) -> Result<Option<Change>, E>
where
    R: Repo<Error = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    if entry_mode.is_tree() {
        return Ok(None);
    }

    let path = filename.to_path_lossy().into_owned();

    if entry_mode.is_commit() {
        let diff = if is_addition {
            diff("", &format!("{}", id))
        } else {
            diff(&format!("{}", id), "")
        };
        return Ok(Some(Change {
            file: File::plain(path, mime::TEXT),
            object: Header {
                id: id.detach(),
                size: 0,
                kind: gix::objs::Kind::Commit,
            },
            patch: Patch::Unified(diff),
        }));
    }

    let (guessed, is_text) =
        mime::guess_from_path(&path).map_or((None, false), |m| (Some(m.0), m.1));

    // if the guess is empty, sniff it later after loading from disk
    if guessed.is_some() && !is_text && !entry_mode.is_link() {
        return Ok(Some(Change {
            file: File::plain(path, guessed.unwrap()),
            object: repo.get_header(id.into()).map_err(DiffError::Repo)?,
            patch: Patch::BinaryData,
        }));
    }

    let header = repo.get_header(id.into()).map_err(DiffError::Repo)?;
    if header.size > repo.max_bytes() {
        return Ok(Some(Change {
            file: File::plain(path, guessed.unwrap_or(mime::BINARY)),
            object: header,
            patch: Patch::InputTooLarge,
        }));
    }

    let mut buf = Vec::new();
    repo.load(id.into(), &mut buf).map_err(DiffError::Repo)?;

    // Use the contents to figure out the mime type if the guess
    // was empty
    let (mime, is_text) = if let Some(mime) = guessed {
        (mime, is_text)
    } else {
        tracing::trace!(
            path = tracing::field::debug(&path),
            guess = tracing::field::debug(mime::guess_from_data(&buf)),
            "change: had to sniff to guess mime"
        );
        mime::guess_from_data(&buf)
    };

    if !is_text {
        return Ok(Some(Change {
            file: File::plain(path, mime),
            object: repo.get_header(id.into()).map_err(DiffError::Repo)?,
            patch: Patch::BinaryData,
        }));
    }

    let text = match repo.decode_text(&buf) {
        Ok(decoded) => decoded,
        Err(err) => {
            tracing::warn!(
                err = tracing::field::debug(err),
                path = tracing::field::debug(&path),
                "unable to decode"
            );
            return Ok(Some(Change {
                file: File::plain(path, mime),
                object: repo.get_header(id.into()).map_err(DiffError::Repo)?,
                patch: Patch::BinaryData,
            }));
        }
    };

    let delta = {
        if is_addition {
            diff("", text.as_ref())
        } else {
            diff(text.as_ref(), "")
        }
    };

    Ok(Some(Change {
        file: File::plain(path, mime),
        object: header,
        patch: Patch::Unified(delta),
    }))
}

fn handle_modification<R, E>(
    repo: &R,
    filename: &BStr,
    previous_entry_mode: EntryMode,
    previous_id: gix::Id<'_>,
    entry_mode: EntryMode,
    id: gix::Id<'_>,
) -> Result<(Header, Change), E>
where
    R: Repo<Error = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    debug_assert!(entry_mode.is_blob() && previous_entry_mode.is_blob());
    let path = filename.to_path_lossy().into_owned();
    // let guessed = mime::guess_from_path(&path);
    let (guessed, is_known_text) =
        mime::guess_from_path(&path).map_or((None, false), |m| (Some(m.0), m.1));

    // if the ids are the same, the only thing that
    // happened was a mode change
    if id == previous_id {
        let header = repo.get_header(id.into()).map_err(DiffError::Repo)?;
        return Ok((
            header.clone(),
            Change {
                file: File::plain(path, guessed.unwrap_or(mime::BINARY)),
                object: header,
                patch: Patch::NoChange,
            },
        ));
    }

    // bail if the guessed mime is not text
    // if the guess is empty, sniff it later after loading from disk
    if guessed.is_some() && !is_known_text {
        let before = repo
            .get_header(previous_id.into())
            .map_err(DiffError::Repo)?;
        let after = repo.get_header(id.into()).map_err(DiffError::Repo)?;
        return Ok((
            before,
            Change {
                file: File::plain(path, guessed.unwrap()),
                object: after,
                patch: Patch::BinaryData,
            },
        ));
    }

    let before = repo
        .get_header(previous_id.into())
        .map_err(DiffError::Repo)?;
    let after = repo.get_header(id.into()).map_err(DiffError::Repo)?;

    if before.size > repo.max_bytes() || after.size > repo.max_bytes() {
        return Ok((
            before,
            Change {
                file: File::plain(path, guessed.unwrap_or(mime::BINARY)),
                object: after,
                patch: Patch::InputTooLarge,
            },
        ));
    }

    let mut before_buf = Vec::with_capacity(before.size as usize);

    repo.load(previous_id.detach(), &mut before_buf)
        .map_err(DiffError::Repo)?;

    // sniff the data if guessing from the filename led to nothing
    let (mime, is_text) = if let Some(mime) = guessed {
        (mime, true)
    } else {
        tracing::trace!(
            path = tracing::field::debug(&path),
            "had to sniff to guess mime"
        );
        mime::guess_from_data(&before_buf)
    };

    if !is_text {
        return Ok((
            before,
            Change {
                file: File::plain(path, mime),
                object: after,
                patch: Patch::BinaryData,
            },
        ));
    }
    // XXX is it worth it to check after_buf too?
    let mut after_buf = Vec::with_capacity(after.size as usize);
    repo.load(id.detach(), &mut after_buf)
        .map_err(DiffError::Repo)?;

    let before_text = repo
        .decode_text(&before_buf)
        .map_err(|e| DiffError::Repo(e))?;
    let after_text = repo.decode_text(&after_buf).map_err(DiffError::Repo)?;

    let delta = diff(&before_text, &after_text);

    // bytes are not exactly the same but a line-wise diff led to
    // no diff
    if delta.is_empty() {
        Ok((
            before,
            Change {
                file: File::plain(path, mime),
                object: after,
                patch: Patch::NoChange,
            },
        ))
    } else {
        Ok((
            before,
            Change {
                file: File::plain(path, mime),
                object: after,
                patch: Patch::Unified(delta),
            },
        ))
    }
}

#[derive(Debug)]
pub enum Event {
    Addition(Change),
    Deletion(Change),
    Modification {
        src: Header,
        change: Change,
    },
    // it's a modification with an attached
    // `src_path`
    Rename {
        src: Header,
        src_path: std::path::PathBuf,
        change: Change,
    },
}

#[derive(Debug)]
pub struct Change {
    pub file: crate::mime::File,
    pub object: Header,
    pub patch: Patch,
}

#[derive(Debug)]
pub enum Patch {
    Unified(UnifiedDiff),
    InputTooLarge,
    BinaryData,
    NoChange,
}

#[derive(Clone, Debug)]
pub struct Header {
    pub id: ObjectId,
    pub size: u64,
    pub kind: gix::object::Kind,
}
