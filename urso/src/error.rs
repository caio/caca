use std::path::PathBuf;

use gix::ObjectId;

// FIXME clone here would make things easier
#[derive(Debug)]
pub enum Error {
    Bug(String),
    InvalidRevSpec(Box<gix::revision::spec::parse::single::Error>),
    ObjectNotFound(ObjectId),
    NotFound,
    Wrapped(WrappedError),
    NotAFile(PathBuf),
    NotADir(PathBuf),
    PathNotRelative(PathBuf),
    Open(Box<gix::open::Error>),
    DetachedHead,
    Peel(ObjectId),
    Decode(ObjectId),
    Header((ObjectId, String)),
    ToString(Box<dyn std::error::Error + 'static + Sync + Send>),
}

// This used to be a PlatformError, with individual discriminants
// for every gix error I would be propagating up, but these
// errors are large and change often so, at least for now,
// I'll go for convenience
#[derive(Debug)]
pub struct WrappedError {
    context: String,
    wrapped: Box<dyn std::error::Error + 'static + Sync + Send>,
}

impl std::fmt::Display for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.context, self.wrapped)
    }
}

impl std::error::Error for WrappedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.wrapped.as_ref())
    }
}

pub(crate) fn wrap_err<E>(msg: String, error: E) -> WrappedError
where
    E: std::error::Error + 'static + Sync + Send,
{
    WrappedError {
        context: msg,
        wrapped: Box::new(error),
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Bug(msg) => write!(f, "BUG: {msg}"),
            Error::InvalidRevSpec(e) => write!(f, "{}", e),
            Error::ObjectNotFound(oid) => write!(f, "object not found: {}", oid),
            Error::NotFound => write!(f, "not found"),
            Error::NotAFile(path) => write!(f, "not a file: {:?}", path),
            Error::NotADir(path) => write!(f, "not a dir: {:?}", path),
            Error::PathNotRelative(path) => write!(f, "path is not relative: {:?}", path),
            Error::Wrapped(w) => write!(f, "unexpected error: {}", w),
            Error::Open(e) => write!(f, "{}", e),
            Error::DetachedHead => write!(f, "repository must have a valid HEAD ref"),
            Error::Peel(id) => write!(f, "failed to peel reference `{}`", id),
            Error::Decode(id) => write!(f, "failed to decode object {}", id),
            Error::Header((oid, msg)) => write!(f, "reading header for {}: {}", oid, msg),
            Error::ToString(inner) => write!(f, "failed to read bytes as string: {}", inner),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::InvalidRevSpec(e) => Some(e),
            Error::Open(e) => Some(e),
            Error::Wrapped(e) => Some(e),
            Error::ToString(e) => Some(e.as_ref()),
            _ => None,
        }
    }
}

impl From<WrappedError> for Error {
    fn from(value: WrappedError) -> Self {
        Error::Wrapped(value)
    }
}

impl From<gix::open::Error> for Error {
    fn from(value: gix::open::Error) -> Self {
        Error::Open(Box::new(value))
    }
}

impl From<gix::revision::spec::parse::single::Error> for Error {
    fn from(value: gix::revision::spec::parse::single::Error) -> Self {
        Self::InvalidRevSpec(Box::new(value))
    }
}
