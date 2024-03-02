#[derive(Debug, Clone, Default, PartialEq, Eq, std::hash::Hash, serde::Serialize)]
pub(crate) struct Metadata {
    pub description: Option<String>,
    pub www: Option<String>,
    pub links: Vec<Link>,
    pub state: State,
}

#[derive(Debug, Clone, PartialEq, Eq, std::hash::Hash, serde::Serialize)]
pub(crate) struct Link {
    name: String,
    href: String,
    title: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, std::hash::Hash, serde::Serialize)]
pub(crate) enum State {
    Archived,
    Default,
    Pinned,
}

impl Default for State {
    fn default() -> Self {
        Self::Default
    }
}

impl State {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "Archived" | "archived" => Some(Self::Archived),
            "Pinned" | "pinned" => Some(Self::Pinned),
            "Default" | "default" => Some(Self::Default),
            _ => None,
        }
    }
}

impl Metadata {
    pub(crate) fn is_any_set(&self) -> bool {
        self != &Self::default()
    }
}

pub(crate) fn read_metadata(
    urso: &urso::Urso,
    rev: &str,
    path: &str,
    buf: &mut Vec<u8>,
) -> Metadata {
    let head = match urso.rev_parse(rev) {
        Ok(head) => head,
        Err(err) => {
            tracing::trace!(?err, "unable to parse rev spec");
            return Metadata::default();
        }
    };

    if let Err(err) = urso.get_file_contents(head, path, buf) {
        tracing::trace!(?err, ?path, "unable to retrieve metadata file");
        return Metadata::default();
    };

    match parse_metadata(buf) {
        Ok(done) => done,
        Err(err) => {
            tracing::error!(?err, ?path, "bad metadata file");
            Metadata::default()
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseError {
    Bug(&'static str),
    Parse(urso::config::Error),
}

impl From<urso::config::Error> for ParseError {
    fn from(value: urso::config::Error) -> Self {
        Self::Parse(value)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Bug(e) => write!(f, "BUG: {}", e),
            ParseError::Parse(w) => w.fmt(f),
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::Parse(w) => Some(w),
            ParseError::Bug(_) => None,
        }
    }
}

pub(crate) fn parse_metadata(data: &[u8]) -> Result<Metadata, ParseError> {
    let mut metadata = Metadata::default();

    let mut err = None;
    urso::config::parse(data, |section, subsection, key, value| -> bool {
        let value = String::from_utf8_lossy(value);
        match section {
            "meta" => {
                if subsection.is_some() {
                    err = Some(ParseError::Bug("meta section must not have subsection"));
                    return false;
                }
                match key {
                    "description" => metadata.description = Some(value.into_owned()),
                    "www" => metadata.www = Some(value.into_owned()),
                    "state" => {
                        let state = State::from_str(&value).unwrap_or_else(|| {
                            tracing::warn!(state = value.as_ref(), "invalid state");
                            State::Default
                        });
                        metadata.state = state;
                    }
                    _ => {
                        tracing::warn!(section, subsection, key, ?value, "unknown key");
                    }
                };
            }
            "link" => {
                let Some(name) = subsection else {
                    err = Some(ParseError::Bug("links must have a subsection"));
                    return false;
                };

                let idx = metadata
                    .links
                    .iter()
                    .position(|l| l.name == name)
                    .unwrap_or_else(|| {
                        metadata.links.push(Link {
                            name: name.to_string(),
                            href: Default::default(),
                            title: None,
                        });
                        metadata.links.len() - 1
                    });

                match key {
                    "href" => metadata.links[idx].href = value.into_owned(),
                    "title" => metadata.links[idx].title = Some(value.into_owned()),
                    _ => {
                        tracing::warn!(section, subsection, key, ?value, "unknown key");
                    }
                };
            }
            // XXX is ignoring unknown best approach?
            _ => {}
        };

        true
    })?;

    if let Some(err) = err {
        Err(err)
    } else {
        Ok(metadata)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn parses_ok() -> Result<(), super::ParseError> {
        // nothing set is valid
        for input in ["", "[meta]", "[unknown]"] {
            let config = super::parse_metadata(input.as_bytes())?;
            assert_eq!(super::Metadata::default(), config);
        }

        // empty value <> omission
        {
            let input = "
                    [meta]
                        description=";
            let config = super::parse_metadata(input.as_bytes())?;
            assert_eq!(Some("".into()), config.description);
        }

        // line escape ok
        {
            let input = "
[meta]
    description = hello \
   world
            www= some\
             thing
";
            let config = super::parse_metadata(input.as_bytes())?;
            assert_eq!(Some("hello world".into()), config.description);
            assert_eq!(Some("something".into()), config.www);
        }

        Ok(())
    }
}
