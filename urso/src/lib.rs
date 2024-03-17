#![forbid(unsafe_code)]
#![deny(unreachable_pub)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::flat_map_option,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::from_iter_instead_of_collect,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_digit_groups,
    clippy::large_stack_arrays,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wild_err_arm,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::missing_enforced_import_renames,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::needless_for_each,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::rc_mutex,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::single_match_else,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]

use error::wrap_err;
use gix::{
    bstr::BStr, object::tree, objs::tree::EntryKind, odb::HeaderExt, prelude::FindExt,
    traverse::commit::Sorting, Object, Repository, Tree,
};

// re-export
pub use gix::{
    actor::SignatureRef,
    date::time::Sign as TimeSign,
    date::Time,
    mailmap::Snapshot as Mailmap,
    objs::{tree::EntryMode, CommitRef, TagRef},
    Commit, ObjectId,
};

use std::{
    borrow::Cow,
    collections::{BinaryHeap, HashSet},
    path::{Path, PathBuf},
};

mod error;
pub use error::Error;

pub mod diff;

pub mod blame;
use blame::Annotated;

mod rename;
use rename::RenameError;

mod mime;
pub use mime::File;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Urso {
    repo: Repository,
    pub max_bytes: u64,
    similarity_threshold: Option<f32>,
}

pub fn guess_mime(path: &str, data: &[u8]) -> (&'static str, bool) {
    mime::guess(path, data)
}

impl std::fmt::Debug for Urso {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Urso")
            .field("repo", &self.git_dir())
            .field("max_bytes", &self.max_bytes)
            .field("similarity_threshold", &self.similarity_threshold)
            .finish()
    }
}

#[derive(Clone)]
pub struct UrsoHandle {
    inner: gix::ThreadSafeRepository,
    max_bytes: u64,
    similarity_threshold: Option<f32>,
}

impl std::fmt::Debug for UrsoHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UrsoHandle")
            .field("repo", &self.inner.git_dir())
            .field("max_bytes", &self.max_bytes)
            .field("similarity_threshold", &self.similarity_threshold)
            .finish()
    }
}

// options are intentionally NOT part of the equation
impl PartialEq for UrsoHandle {
    fn eq(&self, other: &Self) -> bool {
        self.inner.git_dir().eq(other.inner.git_dir())
    }
}

impl UrsoHandle {
    pub fn into_urso(self) -> Urso {
        let UrsoHandle {
            inner,
            max_bytes,
            similarity_threshold,
        } = self;
        Urso {
            repo: inner.into(),
            max_bytes,
            similarity_threshold,
        }
    }

    pub fn git_dir(&self) -> &Path {
        self.inner.git_dir()
    }
}

impl Urso {
    pub fn git_dir(&self) -> &Path {
        self.repo.git_dir()
    }

    pub fn open(
        dir: PathBuf,
        max_bytes: u64,
        similarity_threshold: Option<f32>,
        object_cache_size: usize,
    ) -> Result<Self> {
        let mut repo = gix::ThreadSafeRepository::open(dir)?.to_thread_local();
        repo.object_cache_size(object_cache_size);

        Ok(Self {
            repo,
            max_bytes,
            similarity_threshold,
        })
    }

    pub fn into_handle(self) -> UrsoHandle {
        let Urso {
            repo,
            max_bytes,
            similarity_threshold,
        } = self;

        UrsoHandle {
            inner: repo.into_sync(),
            max_bytes,
            similarity_threshold,
        }
    }

    // finds a blob located at base `path` whose filename matches
    // one of the given `alts`
    // for when you want to get the first of /path/{a,b,c,d,e} that
    // exists in the repo
    // cheaper than machinegunning get_file_contents since it avoids
    // decoding the intermediary path objects multiple times
    // (questionable benefit with tls object decoding cache)
    pub fn read_firstof<P: AsRef<Path>>(
        &self,
        head: ObjectId,
        path: P,
        alts: &[&str],
        buf: &mut Vec<u8>,
    ) -> Result<File> {
        let tree = self.get_commit_tree(head)?;

        // root: just try to find alts
        if path.as_ref().as_os_str().is_empty() {
            for &alt in alts {
                if let Some(found) = tree.find_entry(alt) {
                    self.read_blob(found.object_id(), buf)?;
                    return Ok(File::new(path.as_ref().join(alt), buf));
                }
            }
            return Err(Error::NotFound);
        }

        let Some(entry) = find_path(&path, &tree, buf)? else {
            return Err(Error::NotFound);
        };

        if entry.mode().is_tree() {
            let tree = entry
                .object()
                .map_err(|_discarded| Error::ObjectNotFound(entry.id().detach()))?
                .into_tree();
            for &alt in alts {
                if let Some(found) = tree.find_entry(alt) {
                    buf.clear();
                    self.read_blob(found.object_id(), buf)?;
                    return Ok(File::new(path.as_ref().join(alt), buf));
                }
            }
            Err(Error::NotFound)
        } else {
            Err(Error::NotFound)
        }
    }

    pub fn default_branch(&self) -> Result<String> {
        // XXX swallowing errors
        if let Some(head) = self.repo.head_ref().ok().flatten() {
            Ok(String::from_utf8_lossy(head.name().shorten()).into_owned())
        } else {
            Err(Error::DetachedHead)
        }
    }

    pub fn rev_parse(&self, spec: &str) -> Result<ObjectId> {
        Ok(self
            .repo
            .rev_parse_single(BStr::new(spec.as_bytes()))?
            .detach())
    }

    pub fn diff<F>(&self, base: Commit<'_>, parent: Option<Commit<'_>>, visitor: F) -> Result<()>
    where
        F: FnMut(diff::Event),
    {
        diff::diff_commits(&self, base, parent, visitor).map_err(|err| match err {
            diff::DiffError::Wrapped(w) => Error::Wrapped(w),
            diff::DiffError::Repo(e) => e,
        })
    }

    pub fn list_path<V, P>(&self, head: ObjectId, dir: P, visitor: V) -> Result<()>
    where
        P: AsRef<std::path::Path>,
        V: FnMut(Entry<'_>),
    {
        let tree = self.get_commit_tree(head)?;

        if !dir.as_ref().is_relative() {
            return Err(Error::PathNotRelative(dir.as_ref().into()));
        }

        // empty dir == root of the repo. nothing to find
        if dir.as_ref().as_os_str().is_empty() {
            return list_tree(tree, visitor);
        }

        let mut buf = Vec::new();

        if let Some(entry) = find_path(&dir, &tree, &mut buf)? {
            if !entry.mode().is_tree() {
                Err(Error::NotADir(dir.as_ref().into()))
            } else {
                list_tree(self.find_object(entry.object_id())?.into_tree(), visitor)
            }
        } else {
            Err(Error::NotFound)
        }
    }

    // NEEDSWORK: this is expensive AF
    //            its nature is already costly: find the most recent
    //            commit for each file in the tree
    //            worst case IS walking the whole history until the
    //            very first commit
    //            a objectid->commit lookup would be cheap enough, but
    //            i don't control the ingestion, so keeping it up to
    //            date is not trivial
    pub fn list_with_log<V, P>(&self, commit_id: ObjectId, dir: P, mut visitor: V) -> Result<()>
    where
        // F(Entry, id-of-most-recent-commit)
        V: FnMut(Entry<'_>, ObjectId),
        P: AsRef<Path>,
    {
        let mut wanted = Vec::new();
        self.list_path(commit_id, dir.as_ref(), |entry| {
            wanted.push(entry.to_owned());
        })?;

        if wanted.is_empty() {
            return Ok(());
        }

        let mut buf = Vec::new();
        let base = dir.as_ref();
        for rev in self
            .find_commit(commit_id)?
            .ancestors()
            .first_parent_only()
            .sorting(Sorting::ByCommitTimeNewestFirst)
            .all()
            .map_err(|e| wrap_err(format!("walking ancestry of commit {}", commit_id), e))?
            .flatten()
        {
            let commit = self.find_commit(rev.id)?;
            let commit_tree = commit_tree(&commit)?;
            let parent_tree = {
                if let Some(id) = commit.parent_ids().next() {
                    self.get_commit_tree(id.into())?
                } else {
                    // first commit
                    self.repo.empty_tree()
                }
            };

            // from last to first so that swap_remove is safe
            for idx in (0..wanted.len()).rev() {
                let t = &wanted[idx];
                let target = base.join(&t.name);

                let cur = find_path(&target, &commit_tree, &mut buf)?.map(|entry| entry.id());
                let prev = find_path(&target, &parent_tree, &mut buf)?.map(|entry| entry.id());

                if cur != prev {
                    let target = wanted.swap_remove(idx);
                    visitor(target.as_entry(), commit.id);
                }
            }

            if wanted.is_empty() {
                break;
            }
        }

        debug_assert!(wanted.is_empty(), "missing tip commit for {wanted:#?}");

        Ok(())
    }

    // load data into buf; yield the guessed mime and wether it
    // can be treated as text
    // a shortcut for find_header + check-if-blob + guess_mime
    pub fn get_file_contents<P: AsRef<Path>>(
        &self,
        head: ObjectId,
        path: P,
        buf: &mut Vec<u8>,
    ) -> Result<(&'static str, bool)> {
        if path.as_ref().as_os_str().is_empty() {
            return Err(Error::NotFound);
        }

        let tree = self.get_commit_tree(head)?;

        if let Some(entry) = find_path(&path, &tree, buf)? {
            if entry.mode().is_blob() {
                buf.clear();
                self.read_blob(entry.object_id(), buf)?;
                Ok(mime::guess(path, buf))
            } else {
                Err(Error::NotAFile(path.as_ref().into()))
            }
        } else {
            Err(Error::NotFound)
        }
    }

    pub fn annotate(&self, commit: ObjectId, path: PathBuf) -> Result<()> {
        // Must be able to find the given path as a blob
        // using commit as the head
        let mut buf = Vec::new();
        let tree = self.get_commit_tree(commit)?;
        if let Some(e) = find_path(&path, &tree, &mut buf)? {
            if !e.mode().is_blob() {
                return Err(Error::NotAFile(path));
            }
        } else {
            return Err(Error::NotFound);
        }

        let mut versions = Vec::new();
        self.path_rev_walk(
            commit,
            path,
            WalkOptions {
                rename_similarity_threshold: self.similarity_threshold,
                follow_strategy: FollowStrategy::FirstParent,
                stop_when_path_disappears: true,
            },
            |info: Commit<'_>, _: &Path, _prev, cur| -> bool {
                if let Some(ver) = cur {
                    versions.push(Version {
                        commit: info.id,
                        object: ver,
                    });
                }
                true
            },
        )?;

        versions.reverse();

        let out = crate::blame::annotate(&versions[..], self)?;

        let mut lineno = 1;
        let Annotated {
            content,
            annotations,
        } = out;
        for b in annotations {
            let commit = b.id.commit.to_hex_with_len(11);
            for idx in b.lines {
                println!("{} {:3} {}", commit, lineno, content[idx as usize]);
                lineno += 1;
            }
        }

        Ok(())
    }

    // looooooooooong perf tail
    // may walk all the way to the first commit
    pub fn tip<P: AsRef<Path>>(&self, head: ObjectId, path: P) -> Result<ObjectId> {
        let mut found = None;

        // empty path is the root of the repo
        // the most recent commit is whatever head is at
        if path.as_ref().as_os_str().is_empty() {
            return Ok(head);
        }

        self.path_rev_walk(
            head,
            path.as_ref(),
            WalkOptions {
                follow_strategy: FollowStrategy::Default,
                // ensures that the path exists in the
                // tree of the commit `head` points at
                // i.e. exits early when not found instead of
                //      climbing the ancestry
                stop_when_path_disappears: true,
                // want the first commit only
                rename_similarity_threshold: None,
            },
            |info: Commit<'_>, _: &Path, _, _| {
                found = Some(info.id);
                false
            },
        )?;

        if let Some(commit) = found {
            Ok(commit)
        } else {
            Err(Error::NotFound)
        }
    }

    pub fn log<P, F>(&self, head: ObjectId, path: P, visitor: F) -> Result<()>
    where
        P: AsRef<Path>,
        F: FnMut(Commit<'_>, &Path, Option<ObjectId>, Option<ObjectId>) -> bool,
    {
        let tree = self.get_commit_tree(head)?;
        let mut buf = Vec::new();
        if find_entry(path.as_ref(), &tree, &mut buf)?.is_none() {
            return Err(Error::NotFound);
        }

        self.path_rev_walk(
            head,
            path,
            WalkOptions {
                rename_similarity_threshold: self.similarity_threshold,
                follow_strategy: FollowStrategy::Default,
                stop_when_path_disappears: false,
            },
            visitor,
        )?;

        Ok(())
    }

    pub fn local_refs<F>(&self, mut visitor: F) -> Result<()>
    where
        F: FnMut(RefKind<'_>) -> bool,
    {
        'refs: for mut r in self
            .repo
            .references()
            .map_err(|e| wrap_err("preparing to list all refs".into(), e))?
            .all()
            .map_err(|e| wrap_err("iterating over all refs".into(), e))?
            .flatten()
        {
            // XXX this depends on the call order:
            //     peel_to_id_in_place() will overwrite the ref id
            //     with the fully peeled one.
            //     for annotated tags the ref before peeling is
            //     interesting, so must get id before peeling
            let Some(id) = r.try_id().map(|id| id.detach()) else {
                // don't care about symbolic refs
                continue 'refs;
            };
            r.peel_to_id_in_place()
                .map_err(|e| wrap_err("peeling ref".into(), e))?;

            let Some((category, name)) = r.name().category_and_short_name() else {
                continue 'refs;
            };

            match category {
                gix::refs::Category::Tag => {
                    let head = r.inner.peeled.unwrap();
                    // i think i can assume that if head != id, the
                    // tag is annotated
                    match self.find_object(id)?.try_into_tag() {
                        Ok(obj) => {
                            let tag = obj.decode().map_err(|_discarded| Error::Decode(id))?;
                            if !visitor(RefKind::Tag { tag, head }) {
                                break 'refs;
                            }
                        }
                        Err(_ignored) => {
                            if !visitor(RefKind::PlainTag { name, head }) {
                                break 'refs;
                            }
                        }
                    }
                }
                gix::refs::Category::LocalBranch => {
                    if !visitor(RefKind::Branch {
                        name,
                        head: r.id().detach(),
                    }) {
                        break 'refs;
                    }
                }
                _ignored => (),
            };
        }

        Ok(())
    }

    fn find_object(&self, id: ObjectId) -> Result<Object<'_>> {
        self.repo
            .try_find_object(id)
            .map_err(|e| wrap_err(format!("searching for object {}", id), e))?
            .ok_or(Error::ObjectNotFound(id))
    }

    pub fn find_commit(&self, id: ObjectId) -> Result<Commit<'_>> {
        self.find_object(id)?
            .try_into_commit()
            .map_err(|e| wrap_err(format!("reading object {}", id), e).into())
    }

    fn get_commit_tree(&self, id: ObjectId) -> Result<Tree<'_>> {
        commit_tree(&self.find_commit(id)?)
    }

    pub fn read_blob(&self, id: ObjectId, buf: &mut Vec<u8>) -> Result<()> {
        self.repo
            .objects
            .find_blob(&id, buf)
            .map_err(|e| wrap_err(format!("reading blob id {}", id), e))?;

        Ok(())
    }

    pub fn find_header<P: AsRef<Path>>(
        &self,
        head: ObjectId,
        path: P,
    ) -> Result<Option<diff::Header>> {
        let mut tree = self.get_commit_tree(head)?;

        match tree.peel_to_entry_by_path(path.as_ref()) {
            Ok(found) => {
                if let Some(entry) = found {
                    if entry.mode().is_commit() {
                        Ok(Some(diff::Header {
                            id: entry.object_id(),
                            size: 0,
                            kind: gix::objs::Kind::Commit,
                        }))
                    } else {
                        Ok(Some(self.get_header(entry.object_id())?))
                    }
                } else {
                    Ok(None)
                }
            }
            Err(gix::object::find::existing::Error::NotFound { oid }) => {
                Err(Error::ObjectNotFound(oid))
            }
            Err(e) => Err(wrap_err(
                format!("looking for path {:?} at tree {}", path.as_ref(), tree.id),
                e,
            )
            .into()),
        }
    }

    fn compute_similarity(&self, previous: ObjectId, current: ObjectId) -> Result<f32> {
        let mut prev = Vec::new();
        self.read_blob(previous, &mut prev)?;

        let mut cur = Vec::new();
        self.read_blob(current, &mut cur)?;

        let prev_text = self.blob_bytes_to_string(&prev)?;
        let cur_text = self.blob_bytes_to_string(&cur)?;

        let result = diff::similarity(prev_text.as_ref(), cur_text.as_ref());

        Ok(result)
    }

    // XXX fails on symlinks, need to know entry mode before calling
    fn get_header(&self, id: ObjectId) -> Result<diff::Header> {
        let h = self
            .repo
            .objects
            .header(id)
            .map_err(|_discarded| Error::ObjectNotFound(id))?;

        Ok(diff::Header {
            id,
            size: h.size(),
            kind: h.kind(),
        })
    }

    // XXX walks until the beginning if the path doesn't exist
    fn path_rev_walk<P, F>(
        &self,
        head: ObjectId,
        path: P,
        options: WalkOptions,
        mut visitor: F,
    ) -> Result<()>
    where
        P: AsRef<Path>,
        F: PathCommitVisitor,
    {
        let mut path = path.as_ref().to_path_buf();

        let mut queue = OnceQueue::new();

        {
            let commit = self.find_commit(head)?;
            let time = commit.time().expect("handle error");
            queue.insert(ByCommitTime {
                time,
                id: commit.id,
            });
        }

        let mut parent_ids = Vec::new();
        let mut buf = Vec::new();

        while let Some(info) = queue.remove() {
            let ByCommitTime {
                time: _,
                id: commit_id,
            } = info;
            let commit = self.find_commit(commit_id)?;

            let commit_tree = self.get_commit_tree(commit_id)?;
            let entry = find_entry(&path, &commit_tree, &mut buf)?;
            let current = entry.as_ref().map(|e| e.0);

            if options.stop_when_path_disappears && current.is_none() {
                continue;
            }

            parent_ids.clear();
            commit
                .parent_ids()
                .for_each(|id| parent_ids.push(id.detach()));
            let num_parents = parent_ids.len();

            // Decide which parent(s) to follow and yield back the one
            // to compare against
            let parent_id = match num_parents {
                // root commit. no parent, nothing to follow
                0 => {
                    if current.is_some() && !visitor.visit(commit, &path, None, current) {
                        break;
                    }
                    continue;
                }
                // single parent, follow it
                1 => {
                    {
                        let commit = self.find_commit(parent_ids[0])?;
                        let time = commit.time().expect("handle error");
                        queue.insert(ByCommitTime {
                            time,
                            id: commit.id,
                        });
                    }
                    parent_ids[0]
                }
                _merge_commit => {
                    let mut first_treesame_idx = None;

                    for (idx, &parent_id) in parent_ids.iter().enumerate() {
                        let parent_tree = self.get_commit_tree(parent_id)?;

                        let previous = find_path(&path, &parent_tree, &mut buf)?
                            .map(|entry| entry.object_id());

                        if previous == current {
                            first_treesame_idx = Some(idx);
                            break;
                        }
                    }

                    match options.follow_strategy {
                        FollowStrategy::Default => {}
                        FollowStrategy::AllParents => {
                            first_treesame_idx = None;
                        }
                        FollowStrategy::FirstParent => {
                            first_treesame_idx = Some(0);
                        }
                    };

                    if let Some(idx) = first_treesame_idx {
                        let parent_id = parent_ids[idx];
                        {
                            let commit = self.find_commit(parent_id)?;
                            let time = commit.time().expect("handle error");
                            queue.insert(ByCommitTime {
                                time,
                                id: commit.id,
                            });
                        }
                        parent_id
                    } else {
                        for &id in parent_ids.iter() {
                            {
                                let commit = self.find_commit(id)?;
                                let time = commit.time().expect("handle error");
                                queue.insert(ByCommitTime {
                                    time,
                                    id: commit.id,
                                });
                            }
                        }
                        parent_ids[0]
                    }
                }
            };

            let parent_tree = self.get_commit_tree(parent_id)?;

            let mut previous = find_entry(&path, &parent_tree, &mut buf)?.map(|entry| entry.0);

            let mut has_renamed = false;
            if options.rename_similarity_threshold.is_some()
                && previous.is_none()
                // only try to detect renames for blob types
                && entry.is_some_and(|e| matches!(e.1,EntryKind::Blob|EntryKind::BlobExecutable))
            {
                tracing::trace!(
                    commit_id = tracing::field::debug(commit_id),
                    parent_id = tracing::field::debug(parent_id),
                    path = tracing::field::debug(&path),
                    "initiating rename detection"
                );

                if let Some((new_path, new_id)) =
                    rename::find_rename(self, &path, current.unwrap(), &commit_tree, &parent_tree)
                        .map_err(|e| match e {
                        RenameError::Repo(inner) => inner,
                        RenameError::Wrapped(w) => Error::Wrapped(w),
                    })?
                {
                    tracing::debug!(
                        commit_id = tracing::field::debug(commit_id),
                        parent_id = tracing::field::debug(parent_id),
                        new_path = tracing::field::debug(&new_path),
                        old_path = tracing::field::debug(path),
                        "rename detected"
                    );

                    path = new_path;
                    previous = Some(new_id);
                    has_renamed = true;
                } else {
                    tracing::trace!("no rename detected");
                }
            }

            // When a blob is renamed but its contents remain unchanged
            // `previous` and `current` will be the same. Since the commit
            // is likely interesting to the calee, the callback is fired
            if (has_renamed || current != previous)
                && !visitor.visit(commit, &path, previous, current)
            {
                break;
            }
        }

        Ok(())
    }

    // XXX being lossy here can lead to junk for
    //     repositories that hold binary files that can't be
    //     mime-guessed from their name
    #[allow(clippy::unused_self)] // TODO should know how to decode proper
    fn blob_bytes_to_string<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>> {
        Ok(Cow::Borrowed(
            std::str::from_utf8(data).map_err(|e| Error::ToString(Box::new(e)))?,
        ))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Version {
    commit: ObjectId,
    object: ObjectId,
}

impl blame::Repo<Version> for &Urso {
    type Error = Error;

    fn load(&self, id: &Version, buf: &mut Vec<u8>) -> std::result::Result<(), Self::Error> {
        self.read_blob(id.object, buf)
    }

    fn decode_text<'a>(
        &self,
        data: &'a [u8],
    ) -> std::result::Result<std::borrow::Cow<'a, str>, Self::Error> {
        self.blob_bytes_to_string(data)
    }
}

impl diff::Repo for &Urso {
    type Error = Error;

    fn max_bytes(&self) -> u64 {
        self.max_bytes
    }

    fn empty_tree(&self) -> Tree<'_> {
        self.repo.empty_tree()
    }

    fn load(&self, id: ObjectId, buf: &mut Vec<u8>) -> std::result::Result<(), Self::Error> {
        self.read_blob(id, buf)
    }

    fn get_header(&self, id: ObjectId) -> std::result::Result<diff::Header, Self::Error> {
        Urso::get_header(self, id)
    }

    fn decode_text<'a>(
        &self,
        data: &'a [u8],
    ) -> std::result::Result<std::borrow::Cow<'a, str>, Self::Error> {
        self.blob_bytes_to_string(data)
    }

    fn min_similarity(&self) -> Option<f32> {
        self.similarity_threshold
    }
}

impl rename::Repo for &Urso {
    type Error = Error;

    fn similarity(&self, prev: ObjectId, cur: ObjectId) -> std::result::Result<f32, Self::Error> {
        self.compute_similarity(prev, cur)
    }

    fn get_header(&self, id: ObjectId) -> std::result::Result<diff::Header, Self::Error> {
        Urso::get_header(self, id)
    }

    fn min_similarity(&self) -> f32 {
        self.similarity_threshold.unwrap_or(1.0)
    }
}

#[derive(Debug)]
struct ByCommitTime {
    time: Time,
    id: ObjectId,
}

impl PartialEq for ByCommitTime {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ByCommitTime {}

impl PartialOrd for ByCommitTime {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ByCommitTime {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.time
            .cmp(&other.time)
            .then_with(|| self.id.cmp(&other.id))
    }
}

struct OnceQueue {
    items: BinaryHeap<ByCommitTime>,
    seen: HashSet<ObjectId>,
}

impl OnceQueue {
    pub(crate) fn new() -> Self {
        Self {
            items: Default::default(),
            seen: Default::default(),
        }
    }

    pub(crate) fn insert(&mut self, info: ByCommitTime) -> bool {
        if self.seen.insert(info.id) {
            self.items.push(info);
            true
        } else {
            false
        }
    }

    pub(crate) fn remove(&mut self) -> Option<ByCommitTime> {
        self.items.pop()
    }
}

#[derive(Default, Clone)]
struct WalkOptions {
    rename_similarity_threshold: Option<f32>,
    follow_strategy: FollowStrategy,
    stop_when_path_disappears: bool,
}

#[derive(PartialEq, Default, Clone)]
pub enum FollowStrategy {
    #[default]
    Default,
    AllParents,
    FirstParent,
}

trait PathCommitVisitor {
    fn visit(
        &mut self,
        commit: Commit<'_>,
        path: &Path,
        previous: Option<ObjectId>,
        current: Option<ObjectId>,
    ) -> bool;
}

impl<F> PathCommitVisitor for F
where
    F: FnMut(Commit<'_>, &Path, Option<ObjectId>, Option<ObjectId>) -> bool,
{
    fn visit(
        &mut self,
        commit: Commit<'_>,
        path: &Path,
        previous: Option<ObjectId>,
        current: Option<ObjectId>,
    ) -> bool {
        (self)(commit, path, previous, current)
    }
}

#[derive(Debug, Clone)]
pub struct Entry<'a> {
    pub mode: EntryMode,
    pub name: &'a [u8],
    pub id: ObjectId,
}

impl<'a> Entry<'a> {
    fn to_owned(&self) -> OwnedEntry {
        OwnedEntry {
            mode: self.mode,
            id: self.id,
            name: String::from_utf8_lossy(self.name).into_owned(),
        }
    }
}

#[derive(Debug)]
struct OwnedEntry {
    mode: EntryMode,
    name: String,
    id: ObjectId,
}

impl OwnedEntry {
    fn as_entry(&self) -> Entry<'_> {
        Entry {
            mode: self.mode,
            id: self.id,
            name: self.name.as_bytes(),
        }
    }
}

fn list_tree<V>(tree: Tree<'_>, mut visitor: V) -> Result<()>
where
    // F(id, name, mode)
    V: FnMut(Entry<'_>),
{
    for maybe_entry in tree.iter() {
        let entry =
            maybe_entry.map_err(|e| wrap_err(format!("listing items for tree {}", tree.id), e))?;
        visitor(Entry {
            mode: entry.mode(),
            name: entry.filename(),
            id: entry.object_id(),
        });
    }
    Ok(())
}

fn commit_tree<'a>(commit: &Commit<'a>) -> Result<Tree<'a>> {
    commit
        .tree()
        .map_err(|e| wrap_err(format!("reading tree for commit {}", commit.id), e).into())
}

fn find_path<'a, P: AsRef<Path>>(
    path: P,
    tree: &Tree<'a>,
    buf: &mut Vec<u8>,
) -> Result<Option<tree::Entry<'a>>> {
    match tree.lookup_entry_by_path(&path, buf) {
        Ok(found) => Ok(found),
        Err(gix::object::find::existing::Error::NotFound { oid }) => {
            Err(Error::ObjectNotFound(oid))
        }
        Err(e) => Err(wrap_err(
            format!("looking for path {:?} at tree {}", path.as_ref(), tree.id),
            e,
        )
        .into()),
    }
}

fn find_entry<P: AsRef<Path>>(
    path: P,
    tree: &Tree<'_>,
    buf: &mut Vec<u8>,
) -> Result<Option<(ObjectId, EntryKind)>> {
    if path.as_ref().as_os_str().is_empty() {
        Ok(Some((tree.id, EntryKind::Tree)))
    } else {
        Ok(find_path(path, tree, buf)?.map(|e| (e.object_id(), e.mode().kind())))
    }
}

#[derive(Debug)]
pub enum RefKind<'a> {
    PlainTag { name: &'a [u8], head: ObjectId },
    Tag { tag: TagRef<'a>, head: ObjectId },
    Branch { name: &'a [u8], head: ObjectId },
}

pub mod config {

    pub use gix::config::parse::Error;
    use gix::config::parse::{from_bytes, Event};

    trait EntryVisitor {
        fn visit(
            &mut self,
            section: &str,
            subsection: Option<&str>,
            key: &str,
            value: &[u8],
        ) -> bool;
    }

    pub fn parse<V>(data: &[u8], mut visitor: V) -> Result<(), Error>
    where
        // F(section, subsection, key, value) -> continue_parsing
        V: FnMut(&str, Option<&str>, &str, &[u8]) -> bool,
    {
        let mut stop = false;

        let mut section = Default::default();
        // not a plain option so i can reuse the buffer
        let mut subsection = Default::default();
        let mut subsection_is_set = false;

        // XXX section names are verified to be alphanumeric and their
        // container implements AsRef<str>, but the accessor erases it
        // by yielding &BStr instead of &Name (or &str)
        macro_rules! assume_str {
            ($bytes:expr) => {
                std::str::from_utf8($bytes).unwrap_or_default()
            };
        }

        let mut key = String::default();
        let mut parial_value = Vec::new();

        from_bytes(data, &mut |event| {
            if stop {
                return;
            }

            match event {
                Event::SectionHeader(head) => {
                    head.name().clone_into(&mut section);
                    subsection_is_set = head.subsection_name().is_some_and(|sub| {
                        sub.clone_into(&mut subsection);
                        true
                    });
                }
                Event::SectionKey(section_key) => {
                    section_key.as_ref().clone_into(&mut key);
                }
                Event::Value(value) => {
                    let mut sub = None;
                    if subsection_is_set {
                        sub = Some(assume_str!(&subsection));
                    }
                    stop = !visitor(assume_str!(&section), sub, &key, &value);
                }
                Event::ValueNotDone(part) => {
                    parial_value.extend_from_slice(&part);
                }
                Event::ValueDone(part) => {
                    parial_value.extend_from_slice(&part);

                    let mut sub = None;
                    if subsection_is_set {
                        sub = Some(assume_str!(&subsection));
                    }
                    stop = !visitor(assume_str!(&section), sub, &key, &parial_value);

                    parial_value.clear();
                }
                _ => (),
            }
        })?;

        Ok(())
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_ok() {
            let input = "
[urso]
    setting = hello \
   world
                 another= HEAD
[many \"a\"]
name = first

[many \"b\"]
name = second
";
            let mut expects = vec![
                ("urso", None, "setting", "hello world"),
                ("urso", None, "another", "HEAD"),
                ("many", Some("a"), "name", "first"),
                ("many", Some("b"), "name", "second"),
            ];

            let visitor = |section: &str, sub: Option<&str>, key: &str, value: &[u8]| -> bool {
                let wanted = expects.remove(0);
                assert_eq!(wanted.0, section, "wrong section");
                assert_eq!(wanted.1, sub, "wrong subsection");
                assert_eq!(wanted.2, key, "wrong key");
                assert_eq!(
                    wanted.3,
                    std::str::from_utf8(value).expect("valid utf8"),
                    "wrong value"
                );
                true
            };

            parse(input.as_bytes(), visitor).expect("no error parsing");

            assert!(expects.is_empty());
        }
    }
}
