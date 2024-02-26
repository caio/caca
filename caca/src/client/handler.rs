use std::{path::PathBuf, sync::Arc};

use axum::{
    http::{header::CONTENT_TYPE, HeaderValue, StatusCode, Uri},
    response::{IntoResponse, Redirect, Response},
};

use urso::{Error, Result, Urso, UrsoHandle};

use crate::{
    repo::{Context, RepoState, Repository},
    view::{render, render_markdown_template, View},
    GlobalConfig,
};

use super::{
    popo::{Command, Popo},
    State,
};

pub(super) struct Handler {
    pub state: State,
    pub config: Arc<GlobalConfig>,
    pub popo: Popo<Blocking, Output>,
    pub reverse_proxy_base: String,
}

impl Handler {
    pub fn catch_up(&mut self, repo: Arc<RepoState>) -> bool {
        tracing::trace!(
            repo = repo.name,
            head = repo.snapshot.head.commit.message.title,
            "received new state"
        );

        self.state.repos.update(repo)
    }

    pub async fn handle(&self, uri: Uri) -> Response {
        match self.route(uri).await {
            // rustfmt formats `([smth],else).func()` horribly
            Output::Markdown(data) => {
                let headers = [(CONTENT_TYPE, HeaderValue::from_static("text/html"))];
                (headers, render_markdown_template(&self.state.env, data)).into_response()
            }
            Output::Serve((mime, data)) => {
                let headers = [(CONTENT_TYPE, HeaderValue::from_static(mime))];
                (headers, data).into_response()
            }
            Output::Static(file) => {
                if let Ok(file) = tokio::fs::File::open(file).await {
                    let body =
                        axum::body::Body::from_stream(tokio_util::io::ReaderStream::new(file));
                    (StatusCode::OK, body).into_response()
                } else {
                    (StatusCode::NOT_FOUND).into_response()
                }
            }
            Output::NotFound => StatusCode::NOT_FOUND.into_response(),
            Output::Error(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg).into_response(),
            Output::Template(tmpl) => render(&self.state.env, tmpl),
            Output::Redirect(location) => Redirect::permanent(&location).into_response(),
        }
    }

    async fn route(&self, uri: Uri) -> Output {
        // request path without the leading slash
        let path = {
            let p = uri.path();
            debug_assert!(p.starts_with('/'));
            &p[1..]
        };

        if !validate_path(path) {
            return Output::NotFound;
        }

        match path {
            "" => {
                return Output::Template(View::index(self.state.repos.listing()));
            }
            "atom.xml" => {
                return Output::Template(View::global_feed(self.state.repos.global_feed()));
            }
            // otherwise it's a request for a repo: continue
            _ => {}
        };

        let Some((repo, repo_uri)) = self.match_repo(path) else {
            return Output::NotFound;
        };

        // Naked uri to the repo, no trailing slash. i.e.: <host>/repo
        if repo_uri.is_empty() && !path.ends_with('/') {
            return Output::Redirect(format!("{}/{path}/", self.reverse_proxy_base));
        }

        // uris look like:
        // <host>:<port>/<repo-name>/<view>/<ctx>?/<path>
        //
        // `ctx` is optional and is what estabilishes the
        // "HEAD" when running git commands
        //
        // it always looks something like:
        //
        // - /branch/<name>
        // - /tag/<name>
        // - /<sha1>
        //
        // so a uri like `example.com/repo/blob/branch/main/a.txt`
        // is requesting the "blob" view, for the "a.txt" file
        // within the "main" branch of the repo.
        //
        // i don't particularly like stuffing this in the path and
        // initially had a `?r=branch/main` param instead; however,
        // that complicated things when rendering user content.
        //
        // e.g.: relative hyperlinks in markdown files would need
        // to (sometimes) preserve the parameter otherwise they'd
        // point at a different version of the resource
        //
        // when the context is in the path this problem doesn't
        // exist because all necessary metadata is in the base
        // path the browser uses for relative urls
        let ((view, rest), had_slash) = repo_uri
            .split_once('/')
            .map_or(((repo_uri, ""), false), |r| (r, true));
        match view {
            "" if rest.is_empty() => Output::Template(View::summary(repo.summary())),

            // tree and www render user markdown; empty path
            // redirects to / so that the (html) url base is stable
            "tree" | "www" if !had_slash => {
                Output::Redirect(format!("{}/{path}/", self.reverse_proxy_base))
            }

            "tree" => self.repo_tree(repo, rest).await,
            "atom.xml" if rest.is_empty() => Output::Template(View::feed(repo.feed())),
            "refs" if rest.is_empty() => Output::Template(View::refs(repo.refs())),
            "blob" if !rest.is_empty() => self.repo_blob(repo, rest).await,
            "raw" if !rest.is_empty() => self.repo_raw(repo, rest).await,
            "log" => self.repo_log(repo, rest).await,
            "www" => self.repo_www(repo, rest).await,
            "commit" => self.repo_commit(repo, rest).await,

            _ => self.repo_catchall(repo, repo_uri),
        }
    }

    async fn repo_tree(&self, repo: &Repository, uri: &str) -> Output {
        let Ok((ctx, path)) = repo.split_context(uri) else {
            return Output::NotFound;
        };

        if path.ends_with('/') || path.is_empty() {
            self.exec(repo, ctx, Exec::Tree(path.to_string())).await
        } else {
            // XXX smells like future regret
            // if the guessed mime is "binary" => redirect to raw
            // if it's textual => redirect to blob
            // so that a hyperlink to a .md file gets the
            // pretty version while also being able to use
            // diplay images with the img tag
            let (_, is_text) = urso::guess_mime(path, &[]);
            if is_text {
                Output::Redirect(repo.blob_url(&ctx, path))
            } else {
                Output::Redirect(repo.raw_url(&ctx, path))
            }
        }
    }

    async fn repo_blob(&self, repo: &Repository, uri: &str) -> Output {
        let Ok((ctx, path)) = repo.split_context(uri) else {
            return Output::NotFound;
        };

        if path.is_empty() {
            return Output::NotFound;
        }

        self.exec(repo, ctx, Exec::Blob(path.to_string())).await
    }

    async fn repo_commit(&self, repo: &Repository, uri: &str) -> Output {
        let Some(ctx) = Context::from_hex(uri) else {
            return Output::NotFound;
        };

        self.exec(repo, ctx, Exec::Show).await
    }

    async fn repo_log(&self, repo: &Repository, uri: &str) -> Output {
        let Ok((ctx, path)) = repo.split_context(uri) else {
            return Output::NotFound;
        };

        self.exec(
            repo,
            ctx,
            Exec::Log((self.config.log_size.get(), path.into())),
        )
        .await
    }

    async fn repo_raw(&self, repo: &Repository, uri: &str) -> Output {
        let Ok((ctx, path)) = repo.split_context(uri) else {
            return Output::NotFound;
        };

        if path.is_empty() {
            return Output::NotFound;
        }

        self.exec(repo, ctx, Exec::Raw(path.to_string())).await
    }

    async fn repo_www(&self, repo: &Repository, uri: &str) -> Output {
        let ctx = {
            // XXX could still allow rendering from any ref
            //     by matching for context afterwards
            if let Some(head) = repo.snapshot.www_head {
                Context::from_id(head)
            } else {
                return Output::NotFound;
            }
        };

        self.exec(repo, ctx, Exec::Www(uri.to_string())).await
    }

    fn repo_catchall(&self, repo: &Repository, rest: &str) -> Output {
        if rest.is_empty() {
            return Output::NotFound;
        }

        if is_dumb_clone(rest) {
            if !self.config.allow_http_clone {
                return Output::NotFound;
            }

            // overzealous: `rest` has been checked for sanity
            // XXX maybe put this assurance in a type eh
            let target = repo.handle.git_dir().join(rest);
            if target.starts_with(repo.handle.git_dir()) {
                return Output::Static(target);
            }
            return Output::NotFound;
        }

        let Ok((ctx, path)) = repo.split_context(rest) else {
            return Output::NotFound;
        };

        // XXX smells like future regret
        // ends with / => assume a tree
        // maybe not text => serve raw
        // otherwise => assume a blob
        if path.is_empty() || path.ends_with('/') {
            Output::Redirect(repo.tree_url(&ctx, path.trim_end_matches('/')))
        } else {
            debug_assert!(!path.is_empty());
            let (_, is_text) = urso::guess_mime(path, &[]);
            if is_text {
                Output::Redirect(repo.blob_url(&ctx, path))
            } else {
                Output::Redirect(repo.raw_url(&ctx, path))
            }
        }
    }

    async fn exec(&self, repo: &Repository, ctx: Context, op: Exec) -> Output {
        let cmd = Blocking {
            repo: repo.state.clone(),
            handle: repo.handle.clone(),
            ctx,
            op,
        };

        match self.popo.execute(cmd).await {
            Ok(output) => output,
            // FIXME better
            Err(_err) => Output::Error("the pool is ded".into()),
        }
    }

    fn match_repo<'u>(&self, uri: &'u str) -> Option<(&Repository, &'u str)> {
        self.state.repos.split_path(uri)
    }
}

fn is_dumb_clone(target: &str) -> bool {
    target == "HEAD"
            || (target.starts_with("info/") && target.len() > 5) // /info/.+
            || (target.starts_with("objects/") && target.len() > 8) // /objects/.+
}

#[derive(Debug, Clone)]
pub(crate) enum Output {
    Markdown(Vec<u8>),
    Serve((&'static str, Vec<u8>)),
    Static(PathBuf),
    NotFound,
    Error(String),
    Template(View),
    Redirect(String),
}

#[derive(Clone)]
pub(crate) struct Blocking {
    repo: Arc<RepoState>,
    handle: UrsoHandle,
    ctx: Context,
    op: Exec,
}

impl std::fmt::Debug for Blocking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("handle", &self.handle)
            .field("ctx", &self.ctx)
            .field("op", &self.op)
            .field("mailmap_version", &self.repo.snapshot.mailmap_version)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Blocking {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
            && self.ctx == other.ctx
            && self.op == other.op
            && self.repo.snapshot == other.repo.snapshot
    }
}

impl Eq for Blocking {}

impl std::hash::Hash for Blocking {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.handle.git_dir().hash(state);
        self.ctx.hash(state);
        self.op.hash(state);
        self.repo.snapshot.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, std::hash::Hash)]
enum Exec {
    Show,
    Tree(String),
    Raw(String),
    Blob(String),
    Log((usize, String)),
    Www(String),
}

impl Command for Blocking {
    type Output = Output;

    fn exec(self) -> Self::Output {
        let Self {
            repo,
            handle,
            ctx,
            op,
        } = self;

        let urso = handle.into_urso();
        match op.run(&urso, ctx, &repo) {
            Ok(output) => output,
            Err(Error::NotFound | Error::NotAFile(_)) => Output::NotFound,
            Err(err) => {
                tracing::error!(?err, "unhandled error within cpu worker");
                Output::Error(format!("cpu worker error: {err}"))
            }
        }
    }
}

impl Exec {
    fn run(self, urso: &Urso, ctx: Context, repo: &RepoState) -> Result<Output> {
        match self {
            Self::Show => repo
                .show_commit(urso, ctx)
                .map(View::commit)
                .map(Output::Template),
            Self::Tree(path) => repo
                .tree(urso, ctx, path)
                .map(View::tree)
                .map(Output::Template),
            Self::Blob(path) => repo
                .blob(urso, ctx, path)
                .map(View::blob)
                .map(Output::Template),
            Self::Log((size, path)) => repo
                .log(urso, ctx, size, path)
                .map(View::log)
                .map(Output::Template),
            Self::Raw(path) => RepoState::raw(urso, ctx, path).map(Output::Serve),
            Self::Www(path) => exec_www(&path, urso, &ctx),
        }
    }
}

fn exec_www(path: &str, urso: &Urso, ctx: &Context) -> urso::Result<Output> {
    let mut data = Vec::new();
    let mime = if path.is_empty() || path.ends_with('/') {
        urso.read_firstof(ctx.head(), path, &["index.md", "index.html"], &mut data)?
            .mime
    } else {
        let (mime, _) = urso.get_file_contents(ctx.head(), path, &mut data)?;
        mime
    };

    if mime == "text/markdown" {
        Ok(Output::Markdown(data))
    } else {
        Ok(Output::Serve((mime, data)))
    }
}

// only reasonable paths are valid.
// assumes already decoded url path
// TODO wrap the in a type before i get bitten maybe
pub(crate) fn validate_path(mut input: &str) -> bool {
    loop {
        if let Some((comp, tail)) = input.split_once('/') {
            if tail.starts_with('/') {
                tracing::trace!(input, "bad uri: repeated slashes detected");
                return false;
            }
            if matches!(comp, "." | "..") {
                tracing::trace!(input, "bad uri: obvious kid shit");
                return false;
            }

            input = tail;
        } else {
            if matches!(input, "." | "..") {
                tracing::trace!(input, "bad uri: obvious kid shit");
                return false;
            }
            break;
        }
    }

    true
}

#[cfg(test)]
mod tests {

    #[test]
    fn path_validation() {
        use super::validate_path;
        assert!(!validate_path("."));
        assert!(!validate_path(".."));
        assert!(!validate_path("a/."));
        assert!(!validate_path("b/./"));
        assert!(!validate_path("/c/../"));
        assert!(!validate_path("/d/../"));
        assert!(!validate_path("//"));
        assert!(!validate_path("//e"));
        assert!(!validate_path("f//"));
    }
}
