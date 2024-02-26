// mime guessing here assumes that version controlled
// stuff is text- if it can't guess based on the filename
// it will sniff the contents (if available) and assume
// plain text if it can't guess anything
// maybe this will bite me one day...

use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub path: PathBuf,
    pub mime: &'static str,
}

pub(crate) const BINARY: &str = "application/octet-stream";

pub(crate) const TEXT: &str = "text/plain";

pub(crate) fn guess_from_path<P: AsRef<Path>>(path: P) -> Option<(&'static str, bool)> {
    let Some(filename) = path.as_ref().file_name() else {
        // shouldn't happen eh
        return Some((BINARY, false));
    };

    // These use extensions as if they were some cute little
    // pointless thing people attach to names
    if filename == "go.mod" || filename == "go.sum" || filename == "Cargo.lock" {
        return Some((TEXT, true));
    }

    // These extensions are mapped incorrectly
    if let Some(ext) = path.as_ref().extension() {
        // TODO fix mime_guess: maps to octect stream
        if ext == "java" {
            return Some(("text/x-java", true));
        }

        // mime_guess has nothing
        if ext == "go" {
            return Some((TEXT, true));
        }
    }

    if let Some(mime) = mime_guess::from_path(path.as_ref()).first_raw() {
        return Some((mime, is_text(mime)));
    }

    // dotfiles (assuming utf8 encoding) are text
    if filename
        .as_encoded_bytes()
        .first()
        .is_some_and(|first_byte| *first_byte == b"."[0])
    {
        return Some((TEXT, true));
    }

    if filename == "CONTRIBUTING"
        || filename == "COPYING"
        || filename == "INSTALL"
        || filename == "LICENSE"
        || filename == "README"
        || filename == "AUTHORS"
        || filename == "readme"
        || filename == "Makefile"
        || filename == "configure"
        || filename == "Dockerfile"
    {
        return Some((TEXT, true));
    }

    None
}

pub(crate) fn guess_from_data(data: &[u8]) -> (&'static str, bool) {
    infer::get(data).map_or((TEXT, true), |m| (m.mime_type(), is_text(m.mime_type())))
}

pub(crate) fn guess<P: AsRef<Path>>(path: P, data: &[u8]) -> (&'static str, bool) {
    guess_from_path(&path).unwrap_or_else(|| {
        if !data.is_empty() {
            tracing::trace!(
                path = tracing::field::debug(path.as_ref()),
                "had to sniff bytes to infer mime"
            );
        }
        guess_from_data(data)
    })
}

fn is_text(mime: &'static str) -> bool {
    let (mime, _param) = mime.split_once(';').unwrap_or((mime, ""));
    let (mime, suffix) = mime.split_once('+').unwrap_or((mime, ""));
    let Some((mtype, subtype)) = mime.split_once('/') else {
        return false;
    };

    if mtype == "text" {
        return true;
    }

    if mtype != "application" {
        return false;
    }

    if matches!(suffix, "xml" | "json") {
        return true;
    }

    if matches!(
        subtype,
        "javascript"
            | "json"
            | "srt"
            | "t"
            | "tk"
            | "xml"
            | "x-sh"
            | "x-tcl"
            | "x-tex"
            | "x-texinfo"
    ) {
        return true;
    }

    false
}

impl File {
    pub(crate) fn plain(path: PathBuf, mime: &'static str) -> Self {
        Self { path, mime }
    }

    pub(crate) fn new(path: PathBuf, data: &[u8]) -> Self {
        let (mime, _) = guess(&path, data);
        Self::plain(path, mime)
    }
}
