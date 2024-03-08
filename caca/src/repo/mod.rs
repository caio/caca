use std::{collections::HashSet, num::NonZeroUsize, sync::Arc};

use chrono::TimeZone;
use urso::{
    diff::{Change, Event, Patch},
    guess_mime, Error, Mailmap, ObjectId, Urso, UrsoHandle,
};

use crate::{
    metadata::{read_metadata, Link, Metadata, State},
    view::{self, render_markdown},
    GlobalConfig,
};

mod feed;
mod id;
mod util;

use feed::{FeedEntry, GlobalFeedEntry, TopK};

pub(crate) use id::HexId;

pub(crate) type DateTime = chrono::DateTime<chrono::FixedOffset>;

#[derive(Clone)]
pub(crate) struct Repository {
    pub handle: UrsoHandle,
    pub state: std::sync::Arc<RepoState>,
}

#[derive(Clone)]
pub(crate) struct RepoState {
    pub name: String,
    pub clone_url: String,
    pub repo_url: String,
    pub feed_base_url: String,
    pub reverse_proxy_base: String,
    pub snapshot: Snapshot,
    // NOTE gix-mailmap::Snapshot could impl debug+hash
    pub mailmap: Mailmap,
    pub feed: Vec<FeedEntry>,
}

impl RepoState {
    pub fn raw(urso: &Urso, ctx: Context, path: String) -> urso::Result<(&'static str, Vec<u8>)> {
        let mut data = Vec::new();
        let (mime, _is_text) = urso.get_file_contents(ctx.head(), &path, &mut data)?;
        Ok((mime, data))
    }

    // this is even worse than info/refs when a repo is large
    pub fn refs(&self) -> Refs<'_> {
        // XXX allocs like a mf could persist

        let branches = self
            .snapshot
            .branches
            .iter()
            .map(|b| BranchRef {
                name: b.name.clone(),
                time: b.commit.author.time,
                time_relative: b.commit.author.time.into(),
                browse_url: self.branch_browse_url(&b.name),
                log_url: self.branch_log_url(&b.name),
            })
            .collect();

        let tags = self
            .snapshot
            .tags
            .iter()
            .map(|t| TagRef {
                name: t.name.clone(),
                annotation: t.annotation.as_ref().map(|a| a.message.clone()),
                time: t.time(),
                time_relative: t.time().into(),
                browse_url: self.tag_browse_url(&t.name),
                log_url: self.tag_log_url(&t.name),
            })
            .collect();
        Refs {
            repo: self.repo_info(),
            branches,
            tags,
        }
    }

    pub fn feed(&self) -> RepoFeed<'_> {
        RepoFeed {
            repo: self.repo_info(),
            updated: self.feed.first().map(|e| e.time()).unwrap_or_default(),
            baseurl: &self.feed_base_url,
            entries: &self.feed[..],
        }
    }

    pub fn log(
        &self,
        urso: &Urso,
        ctx: Context,
        size: usize,
        path: String,
    ) -> urso::Result<Log<'_>> {
        debug_assert!(size > 0);
        let mut entries = Vec::with_capacity(size);
        let mut err = None;
        let mut next = None;

        urso.log(ctx.head(), &path, |commit, path, _prev, _cur| {
            match map_commit(&commit, &self.mailmap) {
                Ok(mapped) => {
                    entries.push(self.commit_as_activity(mapped));
                }
                Err(e) => {
                    err = Some(e);
                    return false;
                }
            };
            // if at capacity, figure out the parent so a
            // pagination url can be generated
            // FIXME not quite right if the boundary is a merge commit
            //       better to take size+1 and trim
            if entries.len() == size {
                if let Some(first_parent) = commit.parent_ids().next() {
                    next = Some((first_parent.detach(), path.display().to_string()));
                }
            }
            entries.len() < size
        })?;

        let next_url = next.map(|(parent, path)| {
            let ctx = Context::from_id(parent.into());
            self.log_url(&ctx, &path)
        });

        if let Some(err) = err {
            Err(err)
        } else {
            Ok(Log {
                repo: self.repo_info(),
                nav: self.build_breadcrumbs(ctx, &path, CrumbKind::Log),
                entries,
                path,
                next_url,
            })
        }
    }

    pub fn new(name: String, urso: &Urso, config: &GlobalConfig) -> urso::Result<Self> {
        let mut buf = Vec::new();

        // XXX could hold this open and clone(), but
        //     then reloading would require process restart
        //     definitely won't add a fswatch for this...
        let mut global_mailmap = None;
        if let Some(ref path) = config.global_mailmap {
            match std::fs::read(path) {
                Ok(data) => {
                    global_mailmap = Some(Mailmap::from_bytes(&data));
                }
                Err(err) => tracing::warn!(?err, "bad global mailmap"),
            }
        }

        let (mailmap_version, mailmap) = urso
            .rev_parse("HEAD")
            .and_then(|id| {
                urso.get_file_contents(id, ".mailmap", &mut buf).map(|_| {
                    tracing::debug!(repo=?urso.git_dir(), object=?id, "mailmap found");
                    (Some(id), Mailmap::from_bytes(&buf))
                })
            })
            .unwrap_or((None, Mailmap::default()));

        let mailmap = global_mailmap
            .map(|mut global| {
                global.merge(mailmap.entries());
                global
            })
            .unwrap_or_default();

        let (tags, branches) = render_refs(urso, &mailmap)?;

        let default_branch = urso.default_branch()?;
        let head = branches
            .iter()
            .find(|b| b.name == default_branch)
            // shouldn't happen eh
            .ok_or_else(|| {
                tracing::error!("unable to find branch {default_branch}");
                Error::DetachedHead
            })?
            .clone();
        let head_id = head.commit.id.id;

        let metadata = if config.metadata_config.enabled {
            buf.clear();
            read_metadata(
                urso,
                config.metadata_config.spec(),
                config.metadata_config.filename(),
                &mut buf,
            )
        } else {
            Metadata::default()
        };

        if metadata.is_any_set() {
            tracing::debug!(repo=?urso.git_dir(), ?metadata, "loaded metadata");
        }

        // XXX could just resolve ref so that HEAD is valid
        let www_head = metadata
            .www
            .as_ref()
            .and_then(|n| branches.iter().find(|b| &b.name == n))
            .map(|b| b.commit.id);

        if metadata.www.is_some() && www_head.is_none() {
            tracing::error!(repo=?urso.git_dir(), ?metadata, "configured www branch does not exist");
        }

        let snapshot = Snapshot {
            head,
            tags,
            branches,
            metadata,
            www_head,
            mailmap_version,
            readme: find_readme(urso, head_id, "")?,
        };

        let clone_url = config.repo_clone_url(&name);
        let repo_url = config.repo_url(&name);

        let reverse_proxy_base = config
            .site
            .reverse_proxy_base
            .as_ref()
            .cloned()
            .unwrap_or_default();

        let mut repo = Self {
            name,
            repo_url,
            clone_url,
            feed_base_url: config.feed_base_url(),
            snapshot,
            mailmap,
            reverse_proxy_base,
            feed: Vec::new(),
        };

        if let Some(k) = config.feed_size {
            repo.build_feed(urso, k)?;
        }

        Ok(repo)
    }

    fn default_context(&self) -> Context {
        Context {
            head: self.snapshot.head.commit.id,
            kind: ContextKind::Branch(self.snapshot.head.name.clone()),
        }
    }

    fn tag_as_activity(&self, tag: Tag) -> feed::TagActivity {
        let (annotation, tagger) = tag
            .annotation
            .map(|ann| (Some(ann.message), Some(ann.author)))
            .unwrap_or_default();

        feed::TagActivity {
            browse_url: self.tag_browse_url(&tag.name),
            tag_name: tag.name,
            tagger,
            annotation,
            commit: self.commit_as_activity(tag.commit),
        }
    }

    fn commit_as_activity(&self, commit: CommitInfo) -> feed::CommitActivity {
        feed::CommitActivity {
            author: commit.author,
            id: commit.id,
            url: self.commit_url(commit.id),
            title: commit.message.title,
            body: commit.message.body,
        }
    }

    fn branch_activity(&self, name: String, commit: CommitInfo) -> feed::BranchActivity {
        feed::BranchActivity {
            is_default_branch: self.snapshot.head.name == name,
            browse_url: self.branch_browse_url(&name),
            branch_name: name,
            commit: self.commit_as_activity(commit),
        }
    }

    fn build_feed(&mut self, urso: &Urso, k: NonZeroUsize) -> urso::Result<()> {
        let mut feed = TopK::new(k);
        // start with tags since there's no need to look further than
        // their tip
        for rev in self.snapshot.tags.iter() {
            if !feed.insert(FeedEntry::Tag(self.tag_as_activity(rev.clone()))) {
                break;
            }
        }

        // commits may appear in multiple branches, but showing
        // the same commit in the feed where just the branch
        // name changed doesn't make much sense; so i don't
        let mut seen = HashSet::new();
        let limit = k.get();
        let mut add_to_feed = |rev: &Branch| -> urso::Result<bool> {
            // when at capacity, can't add anything if `rev`s tip
            // is older than the oldest entry in the feed
            if feed.len() == limit
                && feed.min().expect("feed is at capacity").time() < rev.commit.author.time
            {
                return Ok(false);
            }

            let mut taken = 0;
            let mut err = None;
            urso.log(rev.commit.id.id, "", |commit, _path, _prev, _cur| {
                if !seen.insert(commit.id) {
                    return true;
                }
                match map_commit(&commit, &self.mailmap) {
                    Ok(mapped) => {
                        let entry =
                            FeedEntry::Branch(self.branch_activity(rev.name.clone(), mapped));
                        if !feed.insert(entry) {
                            return false;
                        }
                        taken += 1;
                    }
                    Err(e) => {
                        err = Some(e);
                        return false;
                    }
                };
                taken < limit
            })?;
            if let Some(err) = err {
                Err(err)
            } else {
                Ok(true)
            }
        };

        // so: first load activity from the default branch
        // and keep track of the ids seen. this way only brand
        // new commits on side branches will be listed
        add_to_feed(&self.snapshot.head)?;

        // now walk through the branches, using the tip as a
        // guide to know wether there's any chance it contains
        // recent commits
        for rev in self.snapshot.branches.iter() {
            if rev.name == self.snapshot.head.name {
                continue;
            }
            if !add_to_feed(rev)? {
                tracing::trace!(repo=?urso.git_dir(), branch=?rev.name, "old branch skipped");
                break;
            }
        }

        self.feed = feed.finish();
        Ok(())
    }

    fn repo_pages(&self) -> Pages<'_> {
        // XXX could persist
        let ctx = self.default_context();
        Pages {
            files: self.tree_url(&ctx, ""),
            history: self.log_url(&ctx, ""),
            refs: format!("{}/{}/refs", self.reverse_proxy_base, self.name),
            links: &self.snapshot.metadata.links,
        }
    }

    pub fn summary(&self) -> Summary<'_> {
        Summary {
            repo: self.repo_info(),
            pages: self.repo_pages(),
            head: &self.snapshot.head,
            readme: self.snapshot.readme.as_ref(),
            // XXX could be a config eh
            activity: &self.feed[..self.feed.len().min(10)],
        }
    }

    pub fn show_commit(&self, urso: &Urso, ctx: Context) -> urso::Result<Commit<'_>> {
        debug_assert!(
            matches!(ctx.kind, ContextKind::Commit),
            "bad wiring: only commit context should lead to show"
        );
        let commit = urso.find_commit(ctx.head())?;
        let info = map_commit(&urso.find_commit(ctx.head())?, &self.mailmap)?;

        let (parent, parent_ctx) = if let Some(parent) = commit.parent_ids().next() {
            let pid = parent.detach();
            (
                Some(urso.find_commit(pid)?),
                Some(Context::from_id(pid.into())),
            )
        } else {
            (None, None)
        };

        // FIXME urls here should check for object.kind since
        //       i error404 for submodules/links. could fix that...
        let mut events = Vec::new();
        urso.diff(commit, parent, |event| {
            match event {
                Event::Addition(change) => {
                    let Change {
                        file,
                        object: _,
                        patch,
                    } = change;
                    let path = file.path.to_string_lossy().into_owned();
                    let diff = diff_from_patch(patch, file.mime);
                    let current_url = if matches!(diff, Diff::Image) {
                        Some(self.raw_url(&ctx, &path))
                    } else {
                        Some(self.blob_url(&ctx, &path))
                    };
                    events.push(DiffEvent {
                        kind: DiffEventKind::Created,
                        diff,
                        previous_url: None,
                        current_url,
                        path,
                        old_path: None,
                    });
                }
                Event::Deletion(change) => {
                    let Change {
                        file,
                        object: _,
                        patch,
                    } = change;
                    let path = file.path.to_string_lossy().into_owned();
                    let diff = diff_from_patch(patch, file.mime);
                    let previous_url = if matches!(diff, Diff::Image) {
                        parent_ctx.as_ref().map(|ctx| self.raw_url(ctx, &path))
                    } else {
                        parent_ctx.as_ref().map(|ctx| self.blob_url(ctx, &path))
                    };
                    events.push(DiffEvent {
                        kind: DiffEventKind::Deleted,
                        diff,
                        previous_url,
                        current_url: None,
                        path,
                        old_path: None,
                    });
                }
                Event::Modification { src: _, change } => {
                    let Change {
                        file,
                        object: _,
                        patch,
                    } = change;
                    let path = file.path.to_string_lossy().into_owned();
                    let diff = diff_from_patch(patch, file.mime);
                    let (current_url, previous_url) = if matches!(diff, Diff::Image) {
                        (
                            Some(self.raw_url(&ctx, &path)),
                            parent_ctx.as_ref().map(|ctx| self.raw_url(ctx, &path)),
                        )
                    } else {
                        (
                            Some(self.blob_url(&ctx, &path)),
                            parent_ctx.as_ref().map(|ctx| self.blob_url(ctx, &path)),
                        )
                    };
                    events.push(DiffEvent {
                        kind: DiffEventKind::Modified,
                        diff,
                        previous_url,
                        current_url,
                        path,
                        old_path: None,
                    });
                }
                Event::Rename {
                    src: _,
                    src_path,
                    change,
                } => {
                    let Change {
                        file,
                        object: _,
                        patch,
                    } = change;
                    let src_path = src_path.to_string_lossy().into_owned();
                    let path = file.path.to_string_lossy().into_owned();

                    let diff = diff_from_patch(patch, file.mime);
                    let (current_url, previous_url) = if matches!(diff, Diff::Image) {
                        (
                            Some(self.raw_url(&ctx, &path)),
                            parent_ctx.as_ref().map(|ctx| self.raw_url(ctx, &src_path)),
                        )
                    } else {
                        (
                            Some(self.blob_url(&ctx, &path)),
                            parent_ctx.as_ref().map(|ctx| self.blob_url(ctx, &src_path)),
                        )
                    };

                    events.push(DiffEvent {
                        kind: DiffEventKind::Renamed,
                        diff,
                        previous_url,
                        current_url,
                        path,
                        old_path: Some(src_path),
                    });
                }
            };
        })?;

        Ok(Commit {
            repo: self.repo_info(),
            commit: info,
            events,
        })
    }

    pub fn blob(&self, urso: &Urso, ctx: Context, path: String) -> urso::Result<Blob<'_>> {
        let (kind, content) = get_content(urso, &ctx, &path)?;
        let raw_url = self.raw_url(&ctx, &path);

        let mut num_lines = 0;
        if let Some(ref data) = content {
            num_lines = data.matches('\n').count();
            if !data.ends_with('\n') {
                num_lines += 1;
            }
        }

        let tip = self.tip_from_info(
            &ctx,
            &path,
            render_commit(urso, &self.mailmap, urso.tip(ctx.head(), &path)?)?,
        );

        Ok(Blob {
            repo: self.repo_info(),
            nav: self.build_breadcrumbs(ctx, &path, CrumbKind::Blob),
            kind,
            content: content.unwrap_or_default(),
            raw_url,
            num_lines,
            tip,
            path,
        })
    }

    pub fn tree(&self, urso: &Urso, ctx: Context, path: String) -> urso::Result<Tree<'_>> {
        let mut entries = Vec::new();
        urso.list_path(ctx.head(), &path, |entry| {
            let mode = entry.mode;
            // XXX assuming utf8
            let name = String::from_utf8_lossy(entry.name).into_owned();
            let (kind, url) = {
                if mode.is_tree() {
                    (EntryKind::Dir, self.tree_url_base(&ctx, &path, &name))
                } else if mode.is_link() {
                    (EntryKind::Symlink, "#".into())
                } else if mode.is_commit() {
                    (EntryKind::Submodule, "#".into())
                } else {
                    debug_assert!(mode.is_blob(), "unhandled entry kind {}", mode.as_str());
                    (EntryKind::File, self.blob_url_base(&ctx, &path, &name))
                }
            };

            entries.push(Entry { name, kind, url });
        })?;
        entries.sort_unstable_by(|a, b| a.kind.cmp(&b.kind).then_with(|| a.name.cmp(&b.name)));

        let mut parent_url = None;
        if !path.is_empty() {
            let mut base = path.as_str();
            if path.ends_with('/') {
                base = &base[0..(base.len() - 1)];
            }
            if let Some((parent, _)) = base.rsplit_once('/') {
                parent_url = Some(self.tree_url(&ctx, parent));
            } else {
                parent_url = Some(self.tree_url(&ctx, ""));
            }
        }

        let tip = self.tip_from_info(
            &ctx,
            &path,
            render_commit(urso, &self.mailmap, urso.tip(ctx.head(), &path)?)?,
        );

        let readme = find_readme(urso, ctx.head(), &path)?;

        Ok(Tree {
            repo: self.repo_info(),
            nav: self.build_breadcrumbs(ctx, &path, CrumbKind::Tree),
            path,
            entries,
            parent_url,
            readme,
            tip,
        })
    }

    fn build_breadcrumbs(&self, ctx: Context, path: &str, kind: CrumbKind) -> Breadcrumbs {
        let build_url = |path| match kind {
            CrumbKind::Tree | CrumbKind::Blob => self.tree_url(&ctx, path),
            CrumbKind::Log => self.log_url(&ctx, path),
        };
        let head_url = build_url("");
        let mut components = Vec::new();
        let mut tail = None;
        util::breadcrumbs(path, |crumb| match crumb {
            util::Crumb::Part { name, path } => {
                components.push(Component {
                    value: name.to_string(),
                    url: build_url(path),
                });
            }
            util::Crumb::End { name } => {
                tail = Some(name.to_string());
            }
        });
        Breadcrumbs {
            head: ctx.into(),
            head_url,
            components,
            tail,
            kind,
        }
    }

    pub fn log_url(&self, ctx: &Context, path: &str) -> String {
        debug_assert!(!path.starts_with('/'), "bad input: {path}");
        format!("{}/{}/log{ctx}/{path}", self.reverse_proxy_base, self.name)
    }

    pub fn tree_url(&self, ctx: &Context, path: &str) -> String {
        debug_assert!(!path.ends_with('/'), "must not end with /: {path}");
        if path.is_empty() {
            format!("{}/{}/tree{ctx}/", self.reverse_proxy_base, self.name)
        } else {
            format!(
                "{}/{}/tree{ctx}/{path}/",
                self.reverse_proxy_base, self.name
            )
        }
    }

    pub fn commit_url(&self, id: impl Into<HexId>) -> String {
        format!(
            "{}/{}/commit/{}",
            self.reverse_proxy_base,
            self.name,
            id.into()
        )
    }

    fn tree_url_base(&self, ctx: &Context, base: &str, tail: &str) -> String {
        debug_assert!(!base.starts_with('/'));
        debug_assert!(!tail.ends_with('/'));
        if base.is_empty() {
            format!(
                "{}/{}/tree{ctx}/{tail}/",
                self.reverse_proxy_base, self.name
            )
        } else if base.ends_with('/') {
            format!(
                "{}/{}/tree{ctx}/{base}{tail}/",
                self.reverse_proxy_base, self.name
            )
        } else {
            format!(
                "{}/{}/tree{ctx}/{base}/{tail}/",
                self.reverse_proxy_base, self.name
            )
        }
    }

    fn blob_url_base(&self, ctx: &Context, base: &str, tail: &str) -> String {
        debug_assert!(!base.starts_with('/'));
        debug_assert!(!tail.ends_with('/'));
        if base.is_empty() {
            format!("{}/{}/blob{ctx}/{tail}", self.reverse_proxy_base, self.name)
        } else if base.ends_with('/') {
            format!(
                "{}/{}/blob{ctx}/{base}{tail}",
                self.reverse_proxy_base, self.name
            )
        } else {
            format!(
                "{}/{}/blob{ctx}/{base}/{tail}",
                self.reverse_proxy_base, self.name
            )
        }
    }

    pub fn raw_url(&self, ctx: &Context, path: &str) -> String {
        debug_assert!(!path.starts_with('/'), "bad input: {path}");
        debug_assert!(!path.ends_with('/'), "bad input: {path}");
        format!("{}/{}/raw{ctx}/{path}", self.reverse_proxy_base, self.name)
    }

    pub fn blob_url(&self, ctx: &Context, path: &str) -> String {
        debug_assert!(!path.starts_with('/'), "bad input: {path}");
        debug_assert!(!path.ends_with('/'), "bad input: {path}");
        format!("{}/{}/blob{ctx}/{path}", self.reverse_proxy_base, self.name)
    }

    fn branch_browse_url(&self, name: &str) -> String {
        format!(
            "{}/{}/tree/branch/{name}/",
            self.reverse_proxy_base, self.name
        )
    }

    fn branch_log_url(&self, name: &str) -> String {
        format!(
            "{}/{}/log/branch/{name}/",
            self.reverse_proxy_base, self.name
        )
    }

    fn tag_browse_url(&self, name: &str) -> String {
        format!("{}/{}/tree/tag/{name}/", self.reverse_proxy_base, self.name)
    }

    fn tag_log_url(&self, name: &str) -> String {
        format!("{}/{}/log/tag/{name}/", self.reverse_proxy_base, self.name)
    }

    pub fn idle(&self) -> DateTime {
        let head = self.snapshot.head.commit.author.time;
        if let Some(tag) = self.snapshot.tags.first() {
            let tagged_at = tag.time();
            if tagged_at > head {
                return tagged_at;
            }
        }
        head
    }

    fn repo_info(&self) -> Info<'_> {
        Info {
            name: &self.name,
            url: &self.repo_url,
            clone_url: &self.clone_url,
            description: self.snapshot.metadata.description.as_deref(),
        }
    }

    fn tip_from_info(&self, ctx: &Context, path: &str, info: CommitInfo) -> Tip {
        let log_url = self.log_url(ctx, path);
        let url = self.commit_url(info.id);
        Tip {
            id: info.id,
            author_name: info.author.name,
            message: info.message,
            author_time_relative: info.author.time.into(),
            log_url,
            url,
        }
    }

    fn match_branch<'i>(&self, input: &'i str) -> Option<(&Branch, &'i str)> {
        util::split_first_prefix(input, &self.snapshot.branches[..], |b| &b.name)
    }

    fn match_tag<'i>(&self, input: &'i str) -> Option<(&Tag, &'i str)> {
        util::split_first_prefix(input, &self.snapshot.tags[..], |t| &t.name)
    }

    pub fn split_context<'i>(&self, input: &'i str) -> Result<(Context, &'i str), MatchError<'i>> {
        let original_input = input;
        let (kind, input) = original_input
            .split_once('/')
            .unwrap_or((original_input, ""));

        match kind {
            "branch" => {
                if let Some((branch, input)) = self.match_branch(input) {
                    Ok((
                        Context {
                            head: branch.commit.id,
                            kind: ContextKind::Branch(branch.name.clone()),
                        },
                        input,
                    ))
                } else {
                    Err(MatchError::BranchNotFound(input))
                }
            }
            "tag" => {
                if let Some((tag, input)) = self.match_tag(input) {
                    Ok((
                        Context {
                            head: tag.commit.id,
                            kind: ContextKind::Tag(tag.name.clone()),
                        },
                        input,
                    ))
                } else {
                    Err(MatchError::TagNotFound(input))
                }
            }
            // No reftype kind, maybe it's a commit
            kind => {
                if let Ok(commit_id) = ObjectId::from_hex(kind.as_bytes()) {
                    Ok((Context::from_id(commit_id.into()), input))
                } else {
                    // otherwise, default context and assume the original
                    // input is a parameter for the view
                    Ok((self.default_context(), original_input))
                }
            }
        }
    }
}

fn get_content(
    urso: &Urso,
    ctx: &Context,
    path: &str,
) -> urso::Result<(ContentKind, Option<String>)> {
    // guess the mime without loading data so that there's
    // no need to read stuff that won't be rendered
    let (mime, is_text) = urso::guess_mime(path, &[]);

    // no need to buffer the content for images,
    // just link to it
    if mime.starts_with("image/") {
        return Ok((ContentKind::Image, None));
    }

    // can't render otherwise
    if !is_text {
        return Ok((ContentKind::Other, None));
    }

    let Some(header) = urso.find_header(ctx.head(), path)? else {
        return Err(Error::NotFound);
    };

    if !header.kind.is_blob() {
        return Err(Error::NotFound);
    }

    if header.size > urso.max_bytes {
        return Ok((ContentKind::TooLarge, None));
    }

    // actually load the data. mime guess is more reliable now
    let mut data = Vec::new();
    urso.read_blob(header.id, &mut data)?;
    let (mime, is_text) = urso::guess_mime(path, &data);

    if mime == "text/markdown" {
        Ok((ContentKind::Rendered, Some(render_markdown(&data))))
    } else if is_text {
        match String::from_utf8(data) {
            Ok(valid) => Ok((ContentKind::Text, Some(valid))),
            Err(err) => {
                tracing::warn!(
                    err = tracing::field::debug(err),
                    path = tracing::field::debug(&path),
                    "unable to decode"
                );
                Ok((ContentKind::Other, None))
            }
        }
    } else {
        tracing::warn!(path, mime, "mime guess mismatch: path-based said its text");
        Ok((ContentKind::Other, None))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, std::hash::Hash)]
pub(crate) struct Snapshot {
    pub head: Branch,
    pub metadata: Metadata,
    pub www_head: Option<HexId>,
    pub mailmap_version: Option<ObjectId>,
    pub readme: Option<Readme>,
    pub branches: Vec<Branch>,
    pub tags: Vec<Tag>,
    // sniff the license / spdx tag?
}

#[derive(Clone, Debug, serde::Serialize)]
pub(crate) struct Readme {
    id: HexId,
    path: String,
    content: String,
    mime: &'static str,
}

impl std::hash::Hash for Readme {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Readme {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Eq for Readme {}

// XXX fatal errors only; not found should yield none
fn find_readme<P: AsRef<std::path::Path>>(
    urso: &Urso,
    head: ObjectId,
    basedir: P,
) -> urso::Result<Option<Readme>> {
    let mut entries = Vec::new();
    urso.list_path(head, &basedir, |entry| {
        if entry.mode.is_blob()
            && (entry.name.starts_with(b"README") || entry.name.starts_with(b"readme"))
        {
            // XXX assuming utf8
            let name = String::from_utf8_lossy(entry.name).into_owned();
            entries.push(ReadmeCandidate { id: entry.id, name });
        }
    })?;

    // sort by longest name. lower case comes first
    // so that the last entry is the shortest upper case
    // i.e.: if there's are README, readme, and README-lang
    //       present, README will be the one that gets picked
    entries.sort_unstable_by(|a, b| {
        b.name
            .len()
            .cmp(&a.name.len())
            .then_with(|| b.name.cmp(&a.name))
    });
    tracing::trace!(?entries, "ranked readme files");

    if let Some(entry) = entries.pop() {
        let mut data = Vec::new();
        urso.read_blob(entry.id, &mut data)?;
        let (mime, is_text) = guess_mime(&entry.name, &data);
        tracing::debug!(name = entry.name, mime, "chosen a readme");

        if mime == "text/markdown" {
            Ok(Some(Readme {
                id: entry.id.into(),
                path: entry.name,
                content: view::render_markdown(&data),
                mime,
            }))
        } else if is_text {
            // FIXME might not be utf8
            match String::from_utf8(data) {
                Ok(content) => Ok(Some(Readme {
                    id: entry.id.into(),
                    path: entry.name,
                    content,
                    mime,
                })),
                Err(error) => {
                    tracing::error!(?entry, "decoding readme: {}", error);
                    Ok(None)
                }
            }
        } else {
            tracing::warn!(?entry, "readme not text");
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn render_refs(urso: &Urso, mailmap: &Mailmap) -> urso::Result<(Vec<Tag>, Vec<Branch>)> {
    let mut tags = Vec::new();
    let mut branches = Vec::new();

    let mut err = None;

    urso.local_refs(|refkind| {
        match refkind {
            urso::RefKind::Tag { tag, head } => {
                let Some(tagger) = tag.tagger else {
                    tracing::warn!(
                        repo = tracing::field::debug(urso.git_dir()),
                        name = tracing::field::debug(tag.name),
                        "skipped tag: missing tagger"
                    );
                    return true;
                };

                let commit = match maybe_render_commit(urso, mailmap, head) {
                    Ok(Some(commit)) => commit,
                    Ok(None) => {
                        tracing::warn!(
                            repo = tracing::field::debug(urso.git_dir()),
                            name = tracing::field::debug(tag.name),
                            "skipped tag: non-commit object"
                        );
                        return true;
                    }
                    Err(e) => {
                        err = Some(e);
                        return false;
                    }
                };

                let author = map_signature(tagger, mailmap);
                let annotation = Some(Annotation {
                    author,
                    message: String::from_utf8_lossy(tag.message).into_owned(),
                });

                tags.push(Tag {
                    commit,
                    name: String::from_utf8_lossy(tag.name).into_owned(),
                    annotation,
                });
            }
            urso::RefKind::Branch { name, head } => {
                let name = String::from_utf8_lossy(name).into_owned();

                let commit = match render_commit(urso, mailmap, head) {
                    Ok(commit) => commit,
                    Err(e) => {
                        err = Some(e);
                        return false;
                    }
                };

                branches.push(Branch { commit, name });
            }
            urso::RefKind::PlainTag { name, head } => {
                let name = String::from_utf8_lossy(name).into_owned();

                let commit = match maybe_render_commit(urso, mailmap, head) {
                    Ok(Some(commit)) => commit,
                    Ok(None) => {
                        tracing::warn!(
                            repo = tracing::field::debug(urso.git_dir()),
                            name,
                            "skipped plain tag: non-commit object"
                        );
                        return true;
                    }
                    Err(e) => {
                        err = Some(e);
                        return false;
                    }
                };

                tags.push(Tag {
                    commit,
                    name,
                    annotation: None,
                });
            }
        };
        true
    })?;

    if let Some(err) = err {
        Err(err)
    } else {
        tags.sort_unstable_by_key(|a| std::cmp::Reverse(a.time()));
        branches.sort_unstable_by_key(|a| std::cmp::Reverse(a.commit.author.time));
        Ok((tags, branches))
    }
}

#[derive(Debug)]
pub(crate) enum MatchError<'a> {
    BranchNotFound(&'a str),
    TagNotFound(&'a str),
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct CommitInfo {
    pub id: HexId,
    pub author: Signature,
    pub message: Message,
}

impl std::hash::Hash for CommitInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        // self.author.hash(state);
    }
}

impl PartialEq for CommitInfo {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Eq for CommitInfo {}

impl PartialOrd for CommitInfo {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CommitInfo {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.author
            .time
            .cmp(&other.author.time)
            .then_with(|| self.id.cmp(&other.id))
    }
}

#[derive(Debug, Clone, serde::Serialize)]
struct Tip {
    id: HexId,
    author_name: String,
    message: Message,
    author_time_relative: RelativeDateTime,
    log_url: String,
    url: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub(crate) struct Signature {
    name: String,

    time: DateTime,
    time_relative: RelativeDateTime,

    email: String,
    email_is_url: bool, // eurgh
}

impl std::hash::Hash for Signature {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.time.hash(state);
        self.email.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash, serde::Serialize)]
pub(crate) struct Message {
    pub title: String,
    pub body: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash, serde::Serialize)]
pub(crate) struct Branch {
    pub commit: CommitInfo,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash, serde::Serialize)]
pub struct Annotation {
    pub author: Signature,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub(crate) struct Tag {
    pub commit: CommitInfo,
    pub name: String,
    pub annotation: Option<Annotation>,
}

impl Tag {
    // most recent known time
    fn time(&self) -> DateTime {
        self.annotation
            .as_ref()
            .map_or(self.commit.author.time, |a| a.author.time)
    }
}

impl std::hash::Hash for Tag {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.commit.hash(state);
    }
}

fn convert(t: urso::Time) -> DateTime {
    let offset = if t.sign == urso::TimeSign::Plus {
        chrono::FixedOffset::east_opt(t.offset.abs())
    } else {
        chrono::FixedOffset::west_opt(t.offset.abs())
    };
    offset
        .and_then(|o| o.timestamp_opt(t.seconds, 0).earliest())
        .unwrap_or_else(|| {
            tracing::error!(time=?t, "unable to convert gix::Time to chrono DateTime");
            DateTime::default()
        })
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Pages<'a> {
    files: String,
    history: String,
    refs: String,
    // FIXME metadata.links
    links: &'a [Link],
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Info<'a> {
    name: &'a str,
    description: Option<&'a str>,
    url: &'a str,
    clone_url: &'a str,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Summary<'a> {
    repo: Info<'a>,
    pages: Pages<'a>,
    head: &'a Branch,
    readme: Option<&'a Readme>,
    activity: &'a [FeedEntry],
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Commit<'a> {
    repo: Info<'a>,
    commit: CommitInfo,
    events: Vec<DiffEvent>,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct DiffEvent {
    path: String,
    old_path: Option<String>, // FIXME only set for kind=renamed
    kind: DiffEventKind,
    diff: Diff,
    current_url: Option<String>,
    previous_url: Option<String>,
}

#[derive(Debug, serde::Serialize)]
#[serde(tag = "kind", content = "value")]
enum Diff {
    Unified(Vec<Chunk>),
    NoChange,
    TooLarge,
    Binary,
    Image,
}

#[derive(Debug, serde::Serialize)]
pub(crate) enum DiffEventKind {
    Created,
    Deleted,
    Modified,
    Renamed,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Blob<'a> {
    repo: Info<'a>,
    nav: Breadcrumbs,
    kind: ContentKind,
    content: String,
    raw_url: String,
    num_lines: usize,
    tip: Tip,
    path: String,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub(crate) enum ContentKind {
    Text,
    Image,
    Rendered,
    TooLarge,
    Other,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Tree<'a> {
    repo: Info<'a>,
    // context
    nav: Breadcrumbs,
    path: String,
    entries: Vec<Entry>,
    parent_url: Option<String>,
    readme: Option<Readme>,
    tip: Tip,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub(crate) enum EntryKind {
    Dir,
    File,
    Symlink,
    Submodule,
}

#[derive(Debug, serde::Serialize)]
struct Entry {
    name: String,
    kind: EntryKind,
    url: String,
}

// maybeasin
impl std::ops::Deref for Repository {
    type Target = RepoState;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Listing<'a> {
    title: &'a str,
    header_html: &'a str,
    num_pinned: usize,
    num_archived: usize,
    repos: Vec<ListEntry<'a>>,
}

#[derive(Debug, serde::Serialize)]
struct ListEntry<'a> {
    name: &'a str,
    description: Option<&'a str>,
    state: State,
    idle: DateTime,
    idle_relative: RelativeDateTime,
}

// DateTime that becomes a relative time string
// when serialized. One way.
// XXX One problem I see with this is that copies
//     of the same thing will serialize differently
//     so if you render multiple copies you can end
//     up with: "now", "12 ms ago", "31 ms ago", ...
//     for the exact same contained DateTime
//     ...OK i guess
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct RelativeDateTime(DateTime);

impl From<DateTime> for RelativeDateTime {
    fn from(value: DateTime) -> Self {
        Self(value)
    }
}

impl serde::Serialize for RelativeDateTime {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let relative = format!("{}", chrono_humanize::HumanTime::from(self.0));
        serializer.serialize_str(&relative)
    }
}

pub(crate) struct Repos {
    title: String,
    header_html: String,
    feed_base_url: String,
    inner: Vec<Repository>,
    feed: Vec<GlobalFeedEntry>,
    feed_size: Option<NonZeroUsize>,
}

impl Repos {
    pub fn new(config: &GlobalConfig, inner: Vec<Repository>) -> Self {
        let mut repos = Self {
            feed_base_url: config.feed_base_url(),
            inner,
            feed: Vec::new(),
            feed_size: config.feed_size,
            title: config.site.listing_title.clone(),
            header_html: config.site.listing_html_header.clone(),
        };
        repos.build_global_feed();
        repos
    }

    pub fn listing(&self) -> Listing<'_> {
        // can be kept in memory eh
        let mut listing = Listing {
            num_pinned: 0,
            num_archived: 0,
            repos: Vec::with_capacity(self.inner.len()),
            title: &self.title,
            header_html: &self.header_html,
        };
        for r in self.inner.iter() {
            match r.snapshot.metadata.state {
                State::Archived => listing.num_archived += 1,
                State::Default => {}
                State::Pinned => listing.num_pinned += 1,
            };
            listing.repos.push(ListEntry {
                name: &r.name,
                description: r.snapshot.metadata.description.as_deref(),
                state: r.snapshot.metadata.state,
                idle: r.idle(),
                idle_relative: r.idle().into(),
            });
        }
        listing
            .repos
            .sort_unstable_by_key(|k| std::cmp::Reverse(k.idle));
        listing
    }

    pub fn global_feed(&self) -> GlobalFeed<'_> {
        GlobalFeed {
            updated: self
                .feed
                .first()
                .map(|e| e.entry.time())
                .unwrap_or_default(),
            baseurl: &self.feed_base_url,
            entries: &self.feed,
        }
    }

    // XXX could become add_or_replace() easily
    pub fn update(&mut self, state: Arc<RepoState>) -> bool {
        if let Some(found) = self.inner.iter_mut().find(|r| r.name == state.name) {
            found.state = state;
            self.build_global_feed();
            true
        } else {
            false
        }
    }

    fn build_global_feed(&mut self) {
        let Some(k) = self.feed_size else {
            return;
        };
        let mut topk = TopK::new(k);
        for repo in self.inner.iter() {
            for entry in repo.feed.iter() {
                if !topk.insert(GlobalFeedEntry {
                    repo: repo.name.clone(),
                    entry: entry.clone(),
                }) {
                    break;
                }
            }
        }
        self.feed = topk.finish();
    }

    pub fn split_path<'a>(&self, path: &'a str) -> Option<(&Repository, &'a str)> {
        util::split_first_prefix(path, &self.inner[..], |r| &r.name)
    }
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Log<'a> {
    repo: Info<'a>,
    path: String,
    nav: Breadcrumbs,
    entries: Vec<feed::CommitActivity>,
    next_url: Option<String>,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct Refs<'a> {
    repo: Info<'a>,
    branches: Vec<BranchRef>,
    tags: Vec<TagRef>,
}

#[derive(Debug, serde::Serialize)]
struct BranchRef {
    name: String,
    browse_url: String,
    log_url: String,
    time: DateTime,
    time_relative: RelativeDateTime,
}

#[derive(Debug, serde::Serialize)]
struct TagRef {
    name: String,
    browse_url: String,
    log_url: String,
    annotation: Option<String>,
    time: DateTime,
    time_relative: RelativeDateTime,
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct RepoFeed<'a> {
    repo: Info<'a>,
    updated: DateTime,
    baseurl: &'a str,
    entries: &'a [FeedEntry],
}

#[derive(Debug, serde::Serialize)]
pub(crate) struct GlobalFeed<'a> {
    updated: DateTime,
    baseurl: &'a str,
    entries: &'a [GlobalFeedEntry],
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash)]
pub(crate) struct Context {
    head: HexId,
    kind: ContextKind,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash)]
enum ContextKind {
    Branch(String),
    Tag(String),
    Commit,
}

impl Context {
    pub fn head(&self) -> ObjectId {
        self.head.id
    }

    pub fn from_hex(hex: &str) -> Option<Self> {
        urso::ObjectId::from_hex(hex.as_bytes())
            .ok()
            .map(Into::into)
            .map(Self::from_id)
    }

    pub fn from_id(head: HexId) -> Self {
        Self {
            head,
            kind: ContextKind::Commit,
        }
    }

    fn format_url(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // XXX must never end with a slash
        match self.kind {
            ContextKind::Branch(ref name) => {
                write!(f, "/branch/{name}")
            }
            ContextKind::Tag(ref name) => write!(f, "/tag/{name}"),
            ContextKind::Commit => write!(f, "/{}", self.head),
        }
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format_url(f)
    }
}

#[derive(Debug, serde::Serialize)]
struct Breadcrumbs {
    head: PlainContext,
    // points at the start of the path,
    // when components and tail are empty
    head_url: String,

    kind: CrumbKind,

    // components
    components: Vec<Component>,

    tail: Option<String>,
}

#[derive(Debug, serde::Serialize)]
struct Component {
    value: String,
    url: String,
}

#[derive(Debug, serde::Serialize)]
#[serde(tag = "kind", content = "value")]
enum PlainContext {
    Commit(HexId),
    Branch(String),
    Tag(String),
}

impl From<Context> for PlainContext {
    fn from(value: Context) -> Self {
        match value.kind {
            ContextKind::Branch(name) => PlainContext::Branch(name),
            ContextKind::Tag(name) => PlainContext::Tag(name),
            ContextKind::Commit => PlainContext::Commit(value.head),
        }
    }
}

#[derive(Debug, serde::Serialize)]
enum CrumbKind {
    Tree,
    Log,
    Blob,
}

fn render_commit(urso: &Urso, mailmap: &Mailmap, id: ObjectId) -> urso::Result<CommitInfo> {
    let commit = urso.find_commit(id)?;
    map_commit(&commit, mailmap)
}

// tags usually point at commits, but may point at
// any kind of object (ex: junio-gpg-pub @ git.git)
// for simplicitly, I simply ignore non-commit tags
fn maybe_render_commit(
    urso: &Urso,
    mailmap: &Mailmap,
    id: ObjectId,
) -> urso::Result<Option<CommitInfo>> {
    match urso.find_commit(id) {
        Ok(commit) => map_commit(&commit, mailmap).map(Some),
        Err(err) => {
            tracing::trace!(?err, ?id, "not a commit");
            Ok(None)
        }
    }
}

fn map_commit(commit: &urso::Commit<'_>, mailmap: &Mailmap) -> urso::Result<CommitInfo> {
    let id = commit.id;
    let commit = commit
        .decode()
        .map_err(|_discarded| urso::Error::Decode(id))?;

    // commits have an optional encoding tag. when missing, utf8 is assumed
    // XXX decoder
    if commit.encoding.is_some() {
        tracing::debug!(
            "commit {} encoded with {:?}, decoded as utf-8",
            id,
            String::from_utf8_lossy(commit.encoding.unwrap())
        );
    }

    let author = map_signature(commit.author(), mailmap);

    let msg = commit.message();
    let body = msg
        .body
        .map(|b| String::from_utf8_lossy(b).into_owned())
        .unwrap_or_default();
    let message = Message {
        title: String::from_utf8_lossy(msg.title).trim_end().to_string(),
        body,
    };

    Ok(CommitInfo {
        id: id.into(),
        author,
        message,
    })
}

fn map_signature(sig: urso::SignatureRef<'_>, mailmap: &Mailmap) -> Signature {
    let mut mapped_name = None;
    let mut mapped_email = None;

    if let Some(mapped) = mailmap.try_resolve_ref(sig) {
        mapped_name = mapped.name;
        mapped_email = mapped.email;
    }

    let email = String::from_utf8_lossy(mapped_email.unwrap_or(sig.email)).into_owned();
    let name = String::from_utf8_lossy(mapped_name.unwrap_or(sig.name)).into_owned();

    let mut email_is_url = false;
    if let Ok(valid_url) = url::Url::parse(&email) {
        if matches!(valid_url.scheme(), "http" | "https") {
            email_is_url = true;
        } else {
            tracing::trace!(email, "email url but no likey: {}", valid_url.scheme());
        }
    }

    let time = convert(sig.time);
    Signature {
        name,
        email,
        time_relative: time.into(),
        time,
        email_is_url,
    }
}

fn diff_from_patch(patch: Patch, mime: &str) -> Diff {
    match patch {
        Patch::Unified(diff) => Diff::Unified(diff.chunks.into_iter().map(Into::into).collect()),
        Patch::InputTooLarge => Diff::TooLarge,
        Patch::BinaryData => {
            if mime.starts_with("image/") {
                Diff::Image
            } else {
                Diff::Binary
            }
        }
        Patch::NoChange => Diff::NoChange,
    }
}

#[derive(Debug, serde::Serialize)]
struct Chunk {
    before_start: u32,
    before_len: usize,
    after_start: u32,
    after_len: usize,
    lines: Vec<Line>,
}

impl From<urso::diff::Chunk> for Chunk {
    fn from(value: urso::diff::Chunk) -> Self {
        Self {
            before_start: value.before_pos.start + 1,
            before_len: value.before_pos.len(),
            after_start: value.after_pos.start + 1,
            after_len: value.after_pos.len(),
            lines: value.lines.into_iter().map(|x| x.into()).collect(),
        }
    }
}

#[derive(Debug, serde::Serialize)]
struct Line {
    kind: LineKind,
    sign: char,
    value: String,
}

#[derive(Debug, serde::Serialize)]
enum LineKind {
    Plus,
    Minus,
    Ctx,
}

impl From<urso::diff::Line> for Line {
    fn from(value: urso::diff::Line) -> Self {
        match value {
            urso::diff::Line::Addition(line) => Self {
                kind: LineKind::Plus,
                sign: '+',
                value: line,
            },
            urso::diff::Line::Removal(line) => Self {
                kind: LineKind::Minus,
                sign: '-',
                value: line,
            },
            urso::diff::Line::Context(line) => Self {
                kind: LineKind::Ctx,
                sign: ' ',
                value: line,
            },
        }
    }
}

#[derive(Debug)]
struct ReadmeCandidate {
    id: ObjectId,
    name: String,
}
