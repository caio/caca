// find the first candidate whose name matches the
// prefix of the input. yield the rest
pub(crate) fn split_first_prefix<'c, 'input, T, F>(
    input: &'input str,
    candidates: &'c [T],
    get_name: F,
) -> Option<(&'c T, &'input str)>
where
    F: Fn(&'c T) -> &'c str,
{
    for r in candidates {
        let name = get_name(r);
        if let Some(rest) = input.strip_prefix(name) {
            // it's only a match if there are no more chars
            // or if it starts with the separator ('/')
            if rest.is_empty() {
                return Some((r, rest));
            }
            if let Some(rest) = rest.strip_prefix('/') {
                return Some((r, rest));
            }
            // XXX Could return None here if I require length ordering
            //     But if I get to a point where this matters, there
            //     are better approaches than scanning
        }
    }
    None
}

// visit every component of a (unix)path-like str
// XXX expects sane paths as via client::handler::validate_path
pub(crate) fn breadcrumbs<'a, F>(input: &'a str, mut visitor: F)
where
    F: FnMut(Crumb<'a>),
{
    debug_assert!(!input.starts_with('/'), "path must be relative");
    if input.is_empty() {
        return;
    }

    let input = {
        if input.ends_with('/') {
            &input[0..input.len() - 1]
        } else {
            input
        }
    };

    debug_assert!(!input.is_empty());

    let mut last = 0;
    for (idx, _pat) in input.match_indices('/') {
        debug_assert_ne!(0, idx);
        let name = &input[last..idx];
        let path = &input[0..idx];

        visitor(Crumb::Part { name, path });
        last = idx + 1;
    }

    let name = &input[last..];
    debug_assert!(!name.is_empty());
    visitor(Crumb::End { name });
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Crumb<'a> {
    Part { name: &'a str, path: &'a str },
    End { name: &'a str },
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn match_ref_works() {
        // just a simple wrapper over split_first_prefix
        // match_ref actually came first, which is why
        // this whole thing looks funny
        fn match_ref<'i, 'r>(
            refs: &'r [&'r str],
            input: &'i str,
        ) -> Option<(&'r &'r str, &'i str)> {
            split_first_prefix(input, refs, |r| r)
        }

        let refs = ["main", "main2", "bob/bugfix", "alice/feature"];

        // can match itself
        for r in refs.iter() {
            let (found, rest) =
                match_ref(&refs[..], r).expect("should be able to match on exact name");
            assert_eq!(r, found, "should find itself");
            assert!(rest.is_empty());
        }

        // splits correctly
        let cases = [
            ("main/", "main", ""),
            ("bob/bugfix/src/hue.rs", "bob/bugfix", "src/hue.rs"),
            // longest wins, assuming precondition
            ("main2/README", "main2", "README"),
            // only one slash is gone after splitting
            ("main//", "main", "/"),
        ];

        for (input, wanted_ref, wanted_rest) in cases.iter() {
            let (found, rest) = match_ref(&refs[..], input).expect("matches something");
            assert_eq!(wanted_ref, found, "bad ref for input: {}", input);
            assert_eq!(*wanted_rest, rest, "bad rest for input: {}", input);
        }

        // doesn't accept partial matches nor junk
        let junk = [
            "/main",
            "master",
            "unknown/thing",
            "",
            " alice/feature",
            "bob/bugfixx",
        ];
        for input in junk.iter() {
            assert!(
                match_ref(&refs[..], input).is_none(),
                "must not think junk is gold"
            );
        }
    }

    fn as_crumbs(input: &str) -> Vec<Crumb<'_>> {
        let mut out = Vec::new();
        breadcrumbs(input, |crumb| out.push(crumb));
        out
    }

    #[test]
    fn crumbs() {
        assert!(as_crumbs("").is_empty());
        assert_eq!(vec![Crumb::End { name: "a" }], as_crumbs("a"));
        assert_eq!(
            vec![
                Crumb::Part {
                    name: "a",
                    path: "a"
                },
                Crumb::End { name: "b" },
            ],
            as_crumbs("a/b")
        );
        assert_eq!(
            vec![
                Crumb::Part {
                    name: "a",
                    path: "a"
                },
                Crumb::Part {
                    name: "b",
                    path: "a/b"
                },
                Crumb::End { name: "c" },
            ],
            as_crumbs("a/b/c")
        );
    }
}
