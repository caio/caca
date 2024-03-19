use std::borrow::Cow;

use gix::diff::blob::{intern::TokenSource, sources::byte_lines};

use crate::diff::{diff, Line, UnifiedDiff};

mod lines;
pub use lines::{Hunk, Lines};

// Receives a sequence of object ids and yields a sequecence
// of ranges to object-id such that every line of the final
// object (the last one in the sequence) is annotated with
// the object id that introduced such line.
pub fn annotate<T, R, E>(ids: &[T], repo: R) -> Result<Annotated<T>, R::Error>
where
    T: Copy + std::fmt::Debug + PartialEq,
    R: Repo<T, Error = E>,
{
    assert!(!ids.is_empty(), "needs at least one id");

    // Shortcut: Single version, just need to know
    // how many lines it has to describe each line
    if ids.len() == 1 {
        let mut buf = Vec::new();
        let id = ids[0];
        repo.load(&id, &mut buf)?;

        let lines = byte_lines(&buf);
        let mut content = Vec::with_capacity(lines.estimate_tokens() as usize);
        for line in lines {
            content.push(String::from_utf8_lossy(line).into());
        }

        return Ok(Annotated {
            annotations: vec![Annotation {
                lines: 0..(content.len() as u32),
                id,
            }],
            content,
        });
    }
    assert!(ids.len() > 1);

    let mut before_buf = Vec::new();
    let mut after_buf = Vec::new();

    // diff the versions pairwise and use it to reconstruct
    // the final state of the blob.
    let mut last_lineno;
    let mut last_id;
    let mut iter = ids[..].windows(2);
    // First pair, setup the state
    let mut state = {
        let Some([prev, cur]) = iter.next() else {
            unreachable!("guaranteed to have at least 2")
        };
        repo.load(prev, &mut before_buf)?;
        let before = repo.decode_text(&before_buf)?;
        repo.load(cur, &mut after_buf)?;
        let after = repo.decode_text(&after_buf)?;

        let delta = diff(&before, &after);
        last_lineno = delta.after_lineno;
        last_id = *cur;
        let mut s = lines::Lines::new(*prev, delta.before_lineno);
        apply_delta(*cur, &mut s, delta);
        s
    };
    // Now apply the rest
    while let Some([prev, cur]) = iter.next() {
        before_buf.clear();
        after_buf.clear();

        repo.load(prev, &mut before_buf)?;
        let before = repo.decode_text(&before_buf)?;
        repo.load(cur, &mut after_buf)?;
        let after = repo.decode_text(&after_buf)?;

        let delta = diff(&before, &after);

        last_lineno = delta.after_lineno;
        last_id = *cur;
        apply_delta(*cur, &mut state, delta);
    }

    let mut state = state.into_inner();
    merge_consecutive(&mut state);

    let mut lineno = 0u32;
    let mut annotations = Vec::new();
    for hunk in state {
        let next = lineno + (hunk.len as u32);
        let lines = lineno..next;
        annotations.push(Annotation { lines, id: hunk.id });
        lineno = next;
    }
    assert_eq!(lineno as usize, last_lineno);

    let mut content = Vec::with_capacity(last_lineno);
    let mut buf_b = Vec::new();
    repo.load(&last_id, &mut buf_b)?;
    let lines = byte_lines(&buf_b);
    for line in lines {
        content.push(String::from_utf8_lossy(line).into());
    }

    Ok(Annotated {
        annotations,
        content,
    })
}

// Since the same version may appear multiple times (say: a commit getting
// reverted), it's possible that `out` now contains consecutive blocks
// where the ids are the same
//
// Merge them so that:
//
// [A{0..1}, A{1..2}, B{2..10}]
//
// becomes:
//
// [A{0..2}, B{2..10}]
fn merge_consecutive<T>(out: &mut Vec<Hunk<T>>)
where
    T: PartialEq + Copy + std::fmt::Debug,
{
    if out.is_empty() {
        return;
    }

    let mut look_at = 0;
    let mut write_at = 0;
    let mut total_merged = 0;

    while look_at < out.len() - 1 {
        let mut j = look_at + 1;
        while j < out.len() {
            // merge nodes sequentially while their ids are the same
            if out[look_at].id == out[j].id {
                out[write_at].len += out[j].len;
                j += 1;
                total_merged += 1;
            } else {
                break;
            }
        }

        write_at += 1;
        look_at = j;

        // a merge happened, so make sure the write_at head
        // looks exactly like the look_at one
        if write_at != look_at && look_at < out.len() {
            out[write_at].len = out[look_at].len;
            out[write_at].id = out[look_at].id;
        }
    }

    if total_merged > 0 {
        out.truncate(out.len() - total_merged);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation<T> {
    pub lines: std::ops::Range<u32>,
    pub id: T,
}

#[derive(Debug, Clone)]
pub struct Annotated<T> {
    pub content: Vec<String>,
    pub annotations: Vec<Annotation<T>>,
}

pub trait Repo<T> {
    type Error: std::error::Error;

    fn load(&self, id: &T, buf: &mut Vec<u8>) -> Result<(), Self::Error>;

    fn decode_text<'a>(&self, data: &'a [u8]) -> std::result::Result<Cow<'a, str>, Self::Error>;
}

fn apply_delta<T>(id: T, state: &mut lines::Lines<T>, patch: UnifiedDiff)
where
    T: std::fmt::Debug + Copy,
{
    // NEEDSWORK: looking back at this after some time away will hurt
    //            could do with some abstraction to make it nicer,
    //            or record all the necessary work during the diffing
    //            so that there's no need to spin? not sure it's worth it...
    #[derive(Debug)]
    enum Op {
        Remove(std::ops::Range<u32>),
        Add(std::ops::Range<u32>),
    }

    let mut offset: isize = 0;
    for chunk in patch.chunks {
        let mut op = None;
        let mut at = (chunk.before_pos.start as isize + offset) as u32;
        assert!(chunk.before_pos.start as isize + offset >= 0);
        for line in &chunk.lines {
            match line {
                Line::Addition(_) => {
                    offset += 1;
                    match op {
                        Some(Op::Remove(range)) => {
                            state.remove(range);
                            op = Some(Op::Add(at..at + 1));
                        }
                        Some(Op::Add(mut range)) => {
                            range.end += 1;
                            op = Some(Op::Add(range));
                        }
                        None => {
                            op = Some(Op::Add(at..at + 1));
                        }
                    };
                }
                Line::Removal(_) => {
                    offset -= 1;
                    match op {
                        Some(Op::Remove(mut range)) => {
                            range.end += 1;
                            op = Some(Op::Remove(range));
                        }
                        Some(Op::Add(range)) => {
                            at += range.len() as u32;
                            state.add(id, range);
                            op = Some(Op::Remove(at..at + 1));
                        }
                        None => {
                            op = Some(Op::Remove(at..at + 1));
                        }
                    };
                }
                Line::Context(_) => {
                    match op {
                        Some(Op::Add(range)) => {
                            at += range.len() as u32;
                            state.add(id, range);
                        }
                        Some(Op::Remove(range)) => {
                            state.remove(range);
                        }
                        None => {}
                    };
                    at += 1;
                    op = None;
                }
            };
        }
        match op {
            Some(Op::Remove(range)) => {
                state.remove(range);
            }
            Some(Op::Add(range)) => {
                state.add(id, range);
            }
            None => {}
        };
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::{borrow::Cow, collections::HashMap};

    impl Repo<u8> for &HashMap<u8, Vec<u8>> {
        type Error = std::convert::Infallible;

        fn load(&self, id: &u8, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
            // there's nothing interesting to test for the error case
            buf.extend_from_slice(&self[id]);
            Ok(())
        }

        fn decode_text<'a>(
            &self,
            data: &'a [u8],
        ) -> std::result::Result<Cow<'a, str>, Self::Error> {
            Ok(String::from_utf8_lossy(data))
        }
    }

    #[test]
    #[should_panic]
    fn panics_with_empty_id_set() {
        annotate(&[], &HashMap::new()).unwrap();
    }

    #[test]
    fn handles_single_id() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc"[..]));
        let res = annotate(&[1], &repo).unwrap().annotations;

        assert_eq!(1, res.len(), "should have a single entry");
        assert_eq!(
            Annotation {
                lines: 0..3,
                id: 1u8
            },
            res[0],
            "should map all 3 lines to a single id"
        );
    }

    #[test]
    fn remove_from_right() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc"[..]));
        repo.insert(2u8, Vec::from(&b"a"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..1,
                id: 1u8
            }],
            res,
        );
    }

    #[test]
    fn remove_from_left() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc\nd"[..]));
        repo.insert(2u8, Vec::from(&b"c\nd"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..2,
                id: 1u8
            }],
            res,
        );
    }

    #[test]
    fn remove_from_middle() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc\nd"[..]));
        repo.insert(2u8, Vec::from(&b"a\nd"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..2,
                id: 1u8
            }],
            res,
        );
    }

    #[test]
    fn add_to_right() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb\nc\nd"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..2,
                    id: 1u8
                },
                Annotation {
                    lines: 2..4,
                    id: 2u8
                }
            ],
            res,
        );
    }

    #[test]
    fn add_to_left() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"c\nd"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb\nc\nd"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..2,
                    id: 2u8
                },
                Annotation {
                    lines: 2..4,
                    id: 1u8
                }
            ],
            res,
        );
    }

    #[test]
    fn add_to_middle_single() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nc"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb\nc"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..1,
                    id: 1u8
                },
                Annotation {
                    lines: 1..2,
                    id: 2u8
                },
                Annotation {
                    lines: 2..3,
                    id: 1u8
                }
            ],
            res,
        );
    }

    #[test]
    fn add_to_middle() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nd"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb\nc\nd"[..]));

        let res = annotate(&[1, 2], &repo).unwrap();
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..1,
                    id: 1u8
                },
                Annotation {
                    lines: 1..3,
                    id: 2u8
                },
                Annotation {
                    lines: 3..4,
                    id: 1u8
                }
            ],
            res.annotations,
            "{:?}",
            res
        );
    }

    #[test]
    fn add_and_remove() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"Z\nb\nZ\nd"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb\nc\nd\ne"[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..1,
                    id: 2u8
                },
                Annotation {
                    lines: 1..2,
                    id: 1u8
                },
                Annotation {
                    lines: 2..3,
                    id: 2u8
                },
                Annotation {
                    lines: 3..4,
                    id: 1u8
                },
                Annotation {
                    lines: 4..5,
                    id: 2u8
                },
            ],
            res,
        );
    }

    #[test]
    fn more_than_two_versions() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb"[..]));
        repo.insert(3u8, Vec::from(&b"a\nb\nc"[..]));
        repo.insert(4u8, Vec::from(&b"a\nb\nc\nd"[..]));

        let res = annotate(&[1, 2, 3, 4], &repo).unwrap();
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..1,
                    id: 1u8
                },
                Annotation {
                    lines: 1..2,
                    id: 2u8
                },
                Annotation {
                    lines: 2..3,
                    id: 3u8
                },
                Annotation {
                    lines: 3..4,
                    id: 4u8
                },
            ],
            res.annotations,
        );

        assert_eq!(vec!["a", "b", "c", "d"], res.content);
    }

    #[test]
    fn removals_do_not_leave_empty_ranges() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc"[..]));
        repo.insert(2u8, Vec::from(&b"a\nd\ne"[..]));

        let res = annotate(&[1, 2, 1], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..3,
                id: 1u8
            },],
            res,
        );
    }

    #[test]
    fn annotate_empty_file() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::new());

        let res = annotate(&[1], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..0,
                id: 1u8
            }],
            res
        );
    }

    #[test]
    fn odd_ranges() {
        let mut repo = HashMap::new();

        let a = b"1
2
3
4
5
6
7
8
9";

        // remove 2; add a,b,c
        // remove 6,7,8; add d
        let b = b"1
a
b
c
3
4
5
d
9";
        repo.insert(1u8, Vec::from(&a[..]));
        repo.insert(2u8, Vec::from(&b[..]));

        let res = annotate(&[1, 2], &repo).unwrap().annotations;
        assert_eq!(
            vec![
                Annotation {
                    lines: 0..1,
                    id: 1u8
                },
                Annotation {
                    lines: 1..4,
                    id: 2u8
                },
                Annotation {
                    lines: 4..7,
                    id: 1u8
                },
                Annotation {
                    lines: 7..8,
                    id: 2u8
                },
                Annotation {
                    lines: 8..9,
                    id: 1u8
                },
            ],
            res,
        );
    }

    #[test]
    fn annotate_merges_consecutive_blocks() {
        let mut repo = HashMap::new();
        repo.insert(1u8, Vec::from(&b"a\nb\nc"[..]));
        repo.insert(2u8, Vec::from(&b"a\nb"[..]));

        let res = annotate(&[1, 2, 1], &repo).unwrap().annotations;
        assert_eq!(
            vec![Annotation {
                lines: 0..3,
                id: 1u8
            },],
            res,
        );
    }

    #[test]
    fn hunk_merging() {
        let mut state = vec![
            Hunk { len: 1, id: 0u8 },
            Hunk { len: 1, id: 1u8 },
            Hunk { len: 3, id: 1u8 },
            Hunk { len: 1, id: 2u8 },
            Hunk { len: 1, id: 3u8 },
            Hunk { len: 2, id: 3u8 },
            Hunk { len: 2, id: 3u8 },
        ];

        merge_consecutive(&mut state);

        assert_eq!(
            vec![
                Hunk { len: 1, id: 0u8 },
                Hunk { len: 4, id: 1u8 },
                Hunk { len: 1, id: 2u8 },
                Hunk { len: 5, id: 3u8 },
            ],
            state
        );
    }

    #[test]
    fn merge_doesnt_mess_tail_up() {
        let mut x = vec![
            Hunk { len: 1, id: 1u8 },
            Hunk { len: 1, id: 1u8 },
            Hunk { len: 1, id: 2u8 },
        ];
        merge_consecutive(&mut x);
        assert_eq!(vec![Hunk { len: 2, id: 1u8 }, Hunk { len: 1, id: 2u8 },], x);
    }
}
