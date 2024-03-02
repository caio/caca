use std::net::SocketAddr;

use std::net::AddrParseError;
use std::net::TcpListener;

use crate::view::Theme;

use std::num::NonZeroUsize;

use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
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

impl Default for GlobalConfig {
    fn default() -> Self {
        Self {
            site: Site {
                listing_title: String::from("caio's code asylum"),
                listing_html_header: String::from("<h1>caca</h1>"),
                base_url: String::from("http://localhost:42080"),
                clone_base_url: None,
                reverse_proxy_base: None,
            },
            max_file_size_bytes: 2 * 1024 * 1024,
            rename_similarity_threshold: Some(0.7),
            repo_object_cache_size: 20 * 1024 * 1024,
            metadata_config: MetadataConfig::default(),
            global_mailmap: None,
            feed_size: NonZeroUsize::new(40),
            log_size: NonZeroUsize::new(30).unwrap(),
            allow_http_clone: true,
            cache_size: NonZeroUsize::new(1000).unwrap(),
            theme: Theme::Static,
            num_threads: None,
            export_all: true, // false => require git-daemon-export-ok
            listen_mode: ListenMode::addr("[::]:42080").expect("valid default socket addr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    pub fn caiodotco() -> Self {
        let config = Self {
            site: Site {
                listing_title: "caio.co/de index".to_string(),
                listing_html_header: "<h1>caio.<strong>co/de</strong></h1>".to_string(),
                base_url: "https://caio.co".to_string(),
                reverse_proxy_base: Some("/de".to_string()),
                clone_base_url: None,
            },
            global_mailmap: Some("/etc/caca/mailmap".into()),
            listen_mode: ListenMode::External,
            ..Default::default()
        };
        config.check().expect("valid live config")
    }

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

    fn from_bytes(data: &[u8]) -> crate::Result<Self> {
        let mut config = Self::default();
        let mut first_err: Option<Box<dyn std::error::Error>> = None;
        urso::config::parse(data, |section, _subsection, key, value| -> bool {
            match section {
                "site" => {
                    match key {
                        "listing-title" => {
                            config.site.listing_title = String::from_utf8_lossy(value).into_owned();
                        }
                        "listing-html-header" => {
                            config.site.listing_html_header =
                                String::from_utf8_lossy(value).into_owned();
                        }
                        "base-url" => {
                            config.site.base_url = String::from_utf8_lossy(value).into_owned();
                        }
                        "clone-base-url" => {
                            config.site.clone_base_url =
                                Some(String::from_utf8_lossy(value).into_owned());
                        }
                        "reverse-proxy-base" => {
                            config.site.reverse_proxy_base =
                                Some(String::from_utf8_lossy(value).into_owned());
                        }
                        _ => {
                            tracing::warn!("discarded unknown key `{key}` in section `{section}`");
                        }
                    };
                }
                "core" => {
                    match key {
                        "theme" => {
                            config.theme =
                                Theme::AutoReload(String::from_utf8_lossy(value).into_owned());
                        }
                        "max-file-size-bytes" => match from_utf8(value) {
                            Ok(max) => config.max_file_size_bytes = max,
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "rename-similarity-threshold" => match from_utf8(value) {
                            Ok(threshold) => {
                                if threshold == 0.0 {
                                    config.rename_similarity_threshold = None;
                                } else if (0.0f32..=1.0).contains(&threshold) {
                                    config.rename_similarity_threshold = Some(threshold);
                                } else {
                                    first_err = Some("threshold must be 0..=1".into());
                                    return false;
                                }
                            }
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "repo-object-cache-size" => match from_utf8(value) {
                            Ok(size) => config.repo_object_cache_size = size,
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "num-threads" => match from_utf8(value) {
                            Ok(val) => config.num_threads = Some(val),
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "allow-http-clone" => match from_utf8(value) {
                            Ok(flag) => config.allow_http_clone = flag,
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "export-all" => match from_utf8(value) {
                            Ok(flag) => config.export_all = flag,
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "cache-size" => match from_utf8(value) {
                            Ok(0) => {
                                first_err = Some("must be non-zero".into());
                                return false;
                            }
                            Ok(size) => {
                                config.cache_size =
                                    NonZeroUsize::new(size).expect("checked for non-zero");
                            }
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        "global-mailmap" => {
                            config.global_mailmap =
                                Some(PathBuf::from(String::from_utf8_lossy(value).as_ref()));
                        }
                        "listen" => match ListenMode::from_bytes(value) {
                            Ok(mode) => config.listen_mode = mode,
                            Err(err) => {
                                first_err = Some(err);
                                return false;
                            }
                        },
                        _ => {
                            tracing::warn!("discarded unknown key `{key}` in section `{section}`");
                        }
                    };
                }
                "metadata" => match key {
                    "spec" => {
                        config.metadata_config.spec =
                            Some(String::from_utf8_lossy(value).into_owned());
                    }
                    "filename" => {
                        config.metadata_config.filename =
                            Some(String::from_utf8_lossy(value).into_owned());
                    }
                    "enabled" => match from_utf8(value) {
                        Ok(flag) => config.metadata_config.enabled = flag,
                        Err(err) => {
                            first_err = Some(err);
                            return false;
                        }
                    },
                    _ => {
                        tracing::warn!("discarded unknown key `{key}` in section `{section}`");
                    }
                },
                _ => {
                    tracing::warn!("discarded unknown key `{key}` in section `{section}`");
                }
            };
            true
        })?;

        config.check()
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BindOptions {
    pub(crate) addr: SocketAddr,
    pub(crate) admin_addr: Option<SocketAddr>,
}

fn from_utf8<T, E>(input: &[u8]) -> crate::Result<T>
where
    T: std::str::FromStr<Err = E>,
    E: 'static + std::error::Error,
{
    Ok(T::from_str(std::str::from_utf8(input)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_is_default() {
        let from_empty = GlobalConfig::from_bytes(&[]).expect("from empty works");
        assert_eq!(GlobalConfig::default(), from_empty);
    }

    #[test]
    fn unknown_is_ignored() {
        let input = "
[unknown-section]
field=value

# known section, unknown field
[site]
garbage=data

# known secion and field, to ensure the parser doesn't bail
# before the end
[core]
theme=/path/to/theme
anotherjunk=12
        ";
        let from_junk =
            GlobalConfig::from_bytes(input.as_bytes()).expect("unknown junk doesn't yield error");
        assert_eq!(
            Theme::AutoReload("/path/to/theme".to_string()),
            from_junk.theme
        );
    }

    #[test]
    fn parse_caiodotco() {
        let live_config = "
[site]
listing-title = caio.co/de index
listing-html-header = <h1>caio.<strong>co/de</strong></h1>
base-url = https://caio.co
reverse-proxy-base = /de

[core]
listen = external
global-mailmap = /etc/caca/mailmap
        ";

        assert_eq!(
            GlobalConfig::caiodotco(),
            GlobalConfig::from_bytes(live_config.as_bytes()).expect("input is valid")
        );
    }
}
