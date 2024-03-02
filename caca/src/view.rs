use std::path::Path;

use axum::{
    http::{header::CONTENT_TYPE, HeaderName, HeaderValue, StatusCode},
    response::IntoResponse,
};
use minijinja::Environment;

use crate::repo::{Blob, Commit, GlobalFeed, Listing, Log, Refs, RepoFeed, Summary, Tree};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Theme {
    Static,
    AutoReload(String),
}

impl Theme {
    pub(crate) fn dir(&self) -> std::io::Result<Option<std::path::PathBuf>> {
        match self {
            Theme::Static => Ok(None),
            Theme::AutoReload(n) => Some(Path::new(n).to_path_buf().canonicalize()).transpose(),
        }
    }

    pub(crate) fn watch_files(&self) -> bool {
        matches!(self, Theme::AutoReload(_))
    }

    pub(crate) fn env(&self) -> Result<minijinja::Environment<'static>, minijinja::Error> {
        match self {
            Theme::Static => static_env(),
            Theme::AutoReload(n) => dir_env(n),
        }
    }
}

pub(crate) fn static_env() -> Result<Environment<'static>, minijinja::Error> {
    let mut env = Environment::new();

    // XXX any non-silly way of making this easier?
    env.add_template("base.html", include_str!("../theme/base.html"))?;
    env.add_template("repo.html", include_str!("../theme/repo.html"))?;
    env.add_template("macros.html", include_str!("../theme/macros.html"))?;

    env.add_template("index.html", include_str!("../theme/index.html"))?;
    env.add_template("summary.html", include_str!("../theme/summary.html"))?;
    env.add_template("tree.html", include_str!("../theme/tree.html"))?;
    env.add_template("blob.html", include_str!("../theme/blob.html"))?;
    env.add_template("log.html", include_str!("../theme/log.html"))?;
    env.add_template("www.html", include_str!("../theme/www.html"))?;
    env.add_template("refs.html", include_str!("../theme/refs.html"))?;
    env.add_template("commit.html", include_str!("../theme/commit.html"))?;
    env.add_template("404.html", include_str!("../theme/404.html"))?;
    env.add_template("500.html", include_str!("../theme/500.html"))?;
    env.add_template("www.html", include_str!("../theme/www.html"))?;

    env.add_template("atom.xml.html", include_str!("../theme/atom.xml.html"))?;
    env.add_template(
        "global_atom.xml.html",
        include_str!("../theme/global_atom.xml.html"),
    )?;

    check_env(&env)?;

    Ok(env)
}

pub(crate) fn dir_env(dir: &str) -> Result<Environment<'static>, minijinja::Error> {
    let mut env = Environment::new();
    env.set_loader(minijinja::path_loader(dir));
    check_env(&env)?;
    Ok(env)
}

fn check_env(env: &Environment<'_>) -> Result<(), minijinja::Error> {
    for kind in Kind::VALUES {
        env.get_template(kind.path())?;
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub(crate) struct View {
    kind: Kind,
    data: minijinja::Value,
}

impl View {
    pub(crate) fn tree(data: Tree<'_>) -> Self {
        Self {
            kind: Kind::Tree,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn commit(data: Commit<'_>) -> Self {
        Self {
            kind: Kind::Commit,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn blob(data: Blob<'_>) -> Self {
        Self {
            kind: Kind::Blob,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn summary(data: Summary<'_>) -> Self {
        Self {
            kind: Kind::Summary,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn index(data: Listing<'_>) -> Self {
        Self {
            kind: Kind::Index,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn feed(data: RepoFeed<'_>) -> Self {
        Self {
            kind: Kind::Feed,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn global_feed(data: GlobalFeed<'_>) -> Self {
        Self {
            kind: Kind::GlobalFeed,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn refs(data: Refs<'_>) -> Self {
        Self {
            kind: Kind::Refs,
            data: minijinja::Value::from_serializable(&data),
        }
    }

    pub(crate) fn log(data: Log<'_>) -> Self {
        Self {
            kind: Kind::Log,
            data: minijinja::Value::from_serializable(&data),
        }
    }
}

#[derive(Debug, Clone)]
enum Kind {
    Index,
    Summary,
    Tree,
    Blob,
    Log,
    Www,
    Commit,
    Refs,
    NotFound,
    Error,
    Feed,
    GlobalFeed,
}

impl Kind {
    const VALUES: [Kind; 12] = [
        Kind::Index,
        Kind::Summary,
        Kind::Tree,
        Kind::Blob,
        Kind::Log,
        Kind::Www,
        Kind::Commit,
        Kind::Refs,
        Kind::NotFound,
        Kind::Error,
        Kind::Feed,
        Kind::GlobalFeed,
    ];

    const fn path(&self) -> &'static str {
        match self {
            Kind::Index => "index.html",
            Kind::Summary => "summary.html",
            Kind::Tree => "tree.html",
            Kind::Blob => "blob.html",
            Kind::Log => "log.html",
            Kind::Www => "www.html",
            Kind::Commit => "commit.html",
            Kind::Refs => "refs.html",
            Kind::NotFound => "404.html",
            Kind::Error => "500.html",
            Kind::Feed => "atom.xml.html",
            Kind::GlobalFeed => "global_atom.xml.html",
        }
    }

    const fn headers(&self) -> [(HeaderName, HeaderValue); 1] {
        if matches!(self, Kind::Feed | Kind::GlobalFeed) {
            [(
                CONTENT_TYPE,
                HeaderValue::from_static("application/atom+xml"),
            )]
        } else {
            [(CONTENT_TYPE, HeaderValue::from_static("text/html"))]
        }
    }
}

pub(crate) fn render(env: &Environment<'_>, view: View) -> axum::response::Response {
    let Ok(tmpl) = env.get_template(view.kind.path()) else {
        return (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("template not found: {}", view.kind.path()),
        )
            .into_response();
    };
    match tmpl.render(view.data) {
        Ok(rendered) => (view.kind.headers(), rendered).into_response(),
        Err(err) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("rendering template: {err:?}"),
        )
            .into_response(),
    }
}

pub(crate) fn render_markdown_template(env: &Environment<'_>, data: Vec<u8>) -> String {
    // split frontmatter
    let (frontmatter, content) = split_frontmatter(&data);

    let mut front = std::collections::HashMap::<String, String>::new();
    if let Some(matter) = frontmatter {
        if let Err(err) = urso::config::parse(matter.data, |section, _sub, key, value| -> bool {
            if section == "page" {
                front.insert(key.to_string(), String::from_utf8_lossy(value).into_owned());
            } else {
                tracing::warn!(
                    section,
                    key,
                    value = String::from_utf8_lossy(value).as_ref(),
                    "unknown frontmatter section"
                );
            }
            true
        }) {
            tracing::warn!(?err, "discarded broken frontmatter");
        };
    }

    // XXX could let markdown use the frontmatter too, but i don't need
    //     it now
    let content = render_markdown(content);

    let Ok(tmpl) = env.get_template(Kind::Www.path()) else {
        // shouldn't happen: startup tests known kinds
        tracing::error!("missing www template. rendering blank");
        return String::default();
    };

    match tmpl.render(minijinja::context! {
        page => front,
        content => content,
    }) {
        Ok(rendered) => rendered,
        Err(err) => {
            tracing::error!(?err, "error rendering user page");
            String::default()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Payload<'a> {
    data: &'a [u8],
    kind: PayloadKind,
}

#[derive(Debug, PartialEq, Eq)]
enum PayloadKind {
    Plus,
    Minus,
}

fn split_frontmatter(data: &[u8]) -> (Option<Payload<'_>>, &[u8]) {
    let (needle, kind) = if data.starts_with(b"+++\n") {
        (b"\n+++\n", PayloadKind::Plus)
    } else if data.starts_with(b"---\n") {
        (b"\n---\n", PayloadKind::Minus)
    } else {
        return (None, data);
    };

    let width = needle.len();
    // start at the newline instead of immediatelly
    // after it so that ---\n---\n is valid
    let rest = &data[3..];

    if let Some(pos) = rest.windows(width).position(|haystack| haystack == needle) {
        // pos == 0 only happens with a dummy zero-length
        // frontmatter.
        // otherwise the offset is 1 to skip the newline
        // character from the starting needle
        let data = if pos == 0 { &[] } else { &rest[1..pos] };
        (Some(Payload { data, kind }), &rest[(pos + width)..])
    } else {
        (None, data)
    }
}

pub(crate) fn render_markdown(data: &[u8]) -> String {
    let mut opts = markdown::Options::gfm();
    opts.compile.allow_dangerous_html = true;
    // XXX can i make use of frontmatter smh
    // opts.parse.constructs.frontmatter = true;
    markdown::to_html_with_options(
        // FIXME assuming markdown files are always utf8 encoded
        &String::from_utf8_lossy(data),
        &opts,
    )
    .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frontmatter_splitting() {
        // nothing to be found
        {
            let (head, tail) = split_frontmatter(b"");
            assert_eq!(head, None);
            assert_eq!(tail, b"");

            let (head, tail) = split_frontmatter(b"+++");
            assert_eq!(head, None);
            assert_eq!(tail, b"+++");

            let (head, tail) = split_frontmatter(b"+++\n++");
            assert_eq!(head, None);
            assert_eq!(tail, b"+++\n++");

            let (head, tail) = split_frontmatter(b"+++\n+++");
            assert_eq!(head, None);
            assert_eq!(tail, b"+++\n+++");

            let (head, tail) = split_frontmatter(b"+++\n +++\n");
            assert_eq!(head, None);
            assert_eq!(tail, b"+++\n +++\n");
        }

        let (head, tail) = split_frontmatter(b"+++\n+++\n");
        assert_eq!(
            head,
            Some(Payload {
                data: b"",
                kind: PayloadKind::Plus
            })
        );
        assert_eq!(tail, b"");

        let (head, tail) = split_frontmatter(b"---\nCACA\n---\nREST");
        assert_eq!(
            head,
            Some(Payload {
                data: b"CACA",
                kind: PayloadKind::Minus
            })
        );
        assert_eq!(tail, b"REST");

        let (head, tail) = split_frontmatter(b"+++\nCACA\n+++\nREST+++\n");
        assert_eq!(
            head,
            Some(Payload {
                data: b"CACA",
                kind: PayloadKind::Plus
            })
        );
        assert_eq!(tail, b"REST+++\n");
    }
}
