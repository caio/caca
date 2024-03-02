use std::net::SocketAddr;

use std::net::AddrParseError;
use std::net::TcpListener;

use crate::view::Theme;

use std::num::NonZeroUsize;

use std::path::PathBuf;

#[derive(Debug, Clone)]
pub(crate) struct GlobalConfig {
    pub site: Site,
    pub max_file_size_bytes: u64,
    pub repo_object_cache_size: usize,
    pub rename_similarity_threshold: Option<f32>,
    pub metadata_config: MetadataConfig,
    pub global_mailmap: Option<PathBuf>,
    pub feed_size: Option<NonZeroUsize>,
    pub log_size: NonZeroUsize,
    pub allow_http_clone: bool,
    pub cache_size: NonZeroUsize,
    pub theme: Theme,
    pub num_threads: Option<usize>,
    pub export_all: bool,
    pub listen_mode: ListenMode,
    // blob_encoding?
}

#[derive(Debug, Clone)]
pub(crate) struct Site {
    pub listing_title: String,
    pub listing_html_header: String,

    pub base_url: String,
    // for mounting it as a subfolder when reverse proxying
    pub reverse_proxy_base: Option<String>,

    // override the url displayed for clone
    // gets the repo name appended
    pub clone_base_url: Option<String>,
}

impl GlobalConfig {
    // XXX iffy but i'm not doing a builder for this thing
    pub fn check(self) -> crate::Result<Self> {
        if self
            .site
            .clone_base_url
            .as_ref()
            .is_some_and(|u| u.ends_with('/'))
        {
            return Err("clone url must not end with slash".into());
        }

        let parsed = url::Url::parse(&self.repo_clone_url("repository"))?;
        if !matches!(parsed.scheme(), "git" | "http" | "https") {
            return Err(format!(
                "clone url scheme must be git or http(s) got: {}",
                parsed.scheme()
            )
            .into());
        }

        if !self.allow_http_clone && parsed.scheme().starts_with("http") {
            return Err("clone url is http but http clone is disabled".into());
        }

        if self
            .site
            .reverse_proxy_base
            .as_ref()
            .is_some_and(|p| !p.starts_with('/') || p.ends_with('/'))
        {
            return Err(
                "reverse proxy base must start with / and not end with it. ex: /valid".into(),
            );
        }

        Ok(self)
    }

    pub fn repo_url(&self, name: &str) -> String {
        format!(
            "{}/{name}",
            self.site.reverse_proxy_base.as_deref().unwrap_or_default()
        )
    }

    pub fn repo_clone_url(&self, name: &str) -> String {
        if let Some(ref url) = self.site.clone_base_url {
            format!("{url}/{name}",)
        } else {
            format!(
                "{}{}/{name}",
                self.site.base_url,
                self.site.reverse_proxy_base.as_deref().unwrap_or_default()
            )
        }
    }

    pub fn feed_base_url(&self) -> String {
        // intentionally not using reverse_proxy_base
        self.site.base_url.clone()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MetadataConfig {
    pub spec: Option<String>,
    pub filename: Option<String>,
    pub enabled: bool,
}

impl Default for MetadataConfig {
    fn default() -> Self {
        Self {
            spec: None,
            filename: None,
            enabled: true,
        }
    }
}

impl MetadataConfig {
    pub(crate) fn spec(&self) -> &str {
        self.spec.as_deref().unwrap_or("HEAD")
    }

    pub(crate) fn filename(&self) -> &str {
        self.filename.as_deref().unwrap_or(".config/caca.ini")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ListenMode {
    External,
    Bind(BindOptions),
}

impl ListenMode {
    // listen = "external"
    // listen = "addr"
    // listen = "addr,admin_addr"
    fn from_bytes(data: &[u8]) -> crate::Result<Self> {
        let text = std::str::from_utf8(data)?.trim();
        if text == "external" {
            Ok(Self::External)
        } else if let Some((addr, admin_addr)) = text.split_once(',') {
            Ok(Self::with_admin(addr.trim(), admin_addr.trim())?)
        } else {
            Ok(Self::addr(text)?)
        }
    }
}

impl ListenMode {
    pub fn external() -> Self {
        Self::External
    }

    pub fn addr(addr: &str) -> std::result::Result<Self, AddrParseError> {
        let addr = addr.parse()?;
        Ok(Self::Bind(BindOptions {
            addr,
            admin_addr: None,
        }))
    }

    pub fn with_admin(addr: &str, admin_addr: &str) -> std::result::Result<Self, AddrParseError> {
        let addr = addr.parse()?;
        let admin_addr = Some(admin_addr.parse()?);
        Ok(Self::Bind(BindOptions { addr, admin_addr }))
    }

    pub fn to_non_blocking_sockets(&self) -> crate::Result<(TcpListener, Option<TcpListener>)> {
        let (app, admin) = match self {
            ListenMode::External => {
                let mut env = listenfd::ListenFd::from_env();
                let app = env
                    .take_tcp_listener(0)?
                    .ok_or("socket activation: need at least one tcp fd from env")?;
                let admin = env.take_tcp_listener(1)?;
                (app, admin)
            }
            ListenMode::Bind(opts) => {
                let app = TcpListener::bind(opts.addr)?;
                let admin = opts.admin_addr.map(TcpListener::bind).transpose()?;
                (app, admin)
            }
        };

        app.set_nonblocking(true)?;
        if let Some(ref admin) = admin {
            admin.set_nonblocking(true)?;
        }

        Ok((app, admin))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BindOptions {
    pub(crate) addr: SocketAddr,
    pub(crate) admin_addr: Option<SocketAddr>,
}
