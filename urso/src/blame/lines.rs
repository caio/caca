use std::ops::Range;

#[derive(Debug, PartialEq)]
pub struct Hunk<T> {
    pub(crate) len: usize,
    pub(crate) id: T,
}

#[derive(Debug, PartialEq)]
pub struct Lines<T>(Vec<Hunk<T>>);

impl<T> Lines<T>
where
    T: Copy + std::fmt::Debug,
{
    pub(crate) fn new(id: T, lineno: usize) -> Self {
        Self(vec![Hunk { len: lineno, id }])
    }

    pub(crate) fn remove(&mut self, removed: Range<u32>) {
        let delta = removed.len();
        debug_assert!(delta > 0);
        match find_range(&self.0, removed) {
            (Pos::Left(start), Pos::Left(end)) => {
                self.0.drain(start..end);
            }
            (Pos::Left(start), Pos::Mid(end, offset)) => {
                self.0[end].len -= offset;
                if end > start {
                    self.0.drain(start..end);
                }
            }
            (Pos::Mid(start, offset), Pos::Left(end)) => {
                self.0[start].len = offset;
                if end > start {
                    self.0.drain((start + 1)..end);
                }
            }
            (Pos::Mid(start, start_offset), Pos::Mid(end, end_offset)) => {
                if start == end {
                    self.0[start].len -= delta;
                } else {
                    self.0[start].len = start_offset;
                    self.0[end].len -= end_offset;
                    if end > start {
                        self.0.drain((start + 1)..end);
                    }
                }
            }
        }
    }

    pub(crate) fn add(&mut self, id: T, added: Range<u32>) {
        debug_assert!(!added.is_empty());
        match find_pos(&self.0, added.start as usize) {
            Pos::Left(idx) => {
                self.0.insert(
                    idx,
                    Hunk {
                        len: added.len(),
                        id,
                    },
                );
            }
            Pos::Mid(idx, offset) => {
                // splitting the node at `idx` in half and inserting
                // a node in the middle
                // XXX can be smarter with the inserts
                let remainder = self.0[idx].len - offset;
                debug_assert!(remainder > 0);
                self.0[idx].len = offset;
                let mid_id = self.0[idx].id;
                self.0.insert(
                    idx + 1,
                    Hunk {
                        len: added.len(),
                        id,
                    },
                );
                self.0.insert(
                    idx + 2,
                    Hunk {
                        len: remainder,
                        id: mid_id,
                    },
                );
            }
        };
    }

    pub(crate) fn into_inner(self) -> Vec<Hunk<T>> {
        self.0
    }
}

impl<T: PartialEq> From<Vec<Hunk<T>>> for Lines<T> {
    fn from(inner: Vec<Hunk<T>>) -> Self {
        assert!(!inner.is_empty());
        for h in &inner {
            assert_ne!(0, h.len);
        }
        Self(inner)
    }
}

impl<T: PartialEq> PartialEq<Vec<Hunk<T>>> for Lines<T> {
    fn eq(&self, other: &Vec<Hunk<T>>) -> bool {
        self.0.eq(other)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Pos {
    Left(usize),       // insert on pos
    Mid(usize, usize), // mitosis (idx, offset)
}

fn find_pos<T>(state: &[Hunk<T>], lineno: usize) -> Pos
where
    T: Copy + std::fmt::Debug,
{
    let mut tally = 0;
    let mut i = 0;
    for hunk in state {
        if tally == lineno {
            return Pos::Left(i);
        }
        if (tally + hunk.len) > lineno {
            let offset = lineno - tally;
            debug_assert!(offset > 0);
            return Pos::Mid(i, offset);
        }
        tally += hunk.len;
        i += 1;
    }
    // XXX maybe errorable? this is not the right place to verify
    //     such expectations
    debug_assert!(tally >= lineno, "trying to find lineno that doesn't exist");
    Pos::Left(i)
}

fn find_range<T>(state: &[Hunk<T>], remove: Range<u32>) -> (Pos, Pos)
where
    T: Copy + std::fmt::Debug,
{
    // NEEDSWORK: the second pos is strictly >= the first one
    //            can do a lot better than using find_pos twice
    (
        find_pos(state, remove.start as usize),
        find_pos(state, remove.end as usize),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn find_pos_expects_valid_lineno() {
        find_pos::<u8>(&[], 9);
    }

    #[test]
    fn find_pos_makes_sense() {
        assert_eq!(Pos::Left(0), find_pos::<u8>(&[], 0));

        let state = vec![
            Hunk { len: 2, id: 1 },
            Hunk { len: 5, id: 2 },
            Hunk { len: 3, id: 1 },
        ];

        assert_eq!(Pos::Left(0), find_pos(&state, 0));
        assert_eq!(Pos::Left(1), find_pos(&state, 2));
        assert_eq!(Pos::Left(2), find_pos(&state, 7));
        assert_eq!(Pos::Left(3), find_pos(&state, 10));
        assert_eq!(Pos::Mid(0, 1), find_pos(&state, 1));
        assert_eq!(Pos::Mid(1, 1), find_pos(&state, 3));
        assert_eq!(Pos::Mid(1, 2), find_pos(&state, 4));
        assert_eq!(Pos::Mid(1, 3), find_pos(&state, 5));
        assert_eq!(Pos::Mid(1, 4), find_pos(&state, 6));
        assert_eq!(Pos::Mid(2, 1), find_pos(&state, 8));
        assert_eq!(Pos::Mid(2, 2), find_pos(&state, 9));
    }

    #[test]
    #[should_panic]
    fn find_range_expects_valid_range() {
        find_range::<u8>(&[], 0..255);
    }

    #[test]
    fn find_range_base_case() {
        assert_eq!((Pos::Left(0), Pos::Left(0)), find_range::<u8>(&[], 0..0));
    }

    #[test]
    fn find_range_makes_sense() {
        let state = vec![
            Hunk { len: 2, id: 1 },
            Hunk { len: 5, id: 2 },
            Hunk { len: 1, id: 3 },
            Hunk { len: 1, id: 1 },
            Hunk { len: 4, id: 1 },
        ];

        assert_eq!((Pos::Left(0), Pos::Mid(0, 1)), find_range(&state, 0..1));
        assert_eq!((Pos::Left(0), Pos::Left(1)), find_range(&state, 0..2));
        assert_eq!((Pos::Left(0), Pos::Mid(1, 1)), find_range(&state, 0..3));
        assert_eq!((Pos::Left(0), Pos::Left(5)), find_range(&state, 0..13));

        assert_eq!((Pos::Mid(1, 4), Pos::Left(2)), find_range(&state, 6..7));
        assert_eq!((Pos::Mid(1, 4), Pos::Left(3)), find_range(&state, 6..8));
        assert_eq!((Pos::Mid(4, 2), Pos::Mid(4, 3)), find_range(&state, 11..12));
    }

    #[test]
    fn lines_initial_state() {
        assert_eq!(Lines::new(1u8, 100), vec![Hunk { id: 1u8, len: 100 }]);
    }

    #[test]
    fn additions() {
        let mut lines: Lines<_> = vec![Hunk { len: 10, id: 0 }].into();

        // addition to the middle of the original hunk
        lines.add(1, 1..3);

        assert_eq!(
            lines,
            vec![
                Hunk { len: 1, id: 0 },
                Hunk { len: 2, id: 1 },
                Hunk { len: 9, id: 0 }
            ],
            "should've split the original hunk"
        );

        // addition to the left of the state (e.g. new lines added to the
        // beginning of the file)
        lines.add(2, 0..5);

        assert_eq!(
            lines,
            vec![
                Hunk { len: 5, id: 2 },
                Hunk { len: 1, id: 0 },
                Hunk { len: 2, id: 1 },
                Hunk { len: 9, id: 0 }
            ],
        );

        // additions to the right of the state
        lines.add(3, 17..21);

        assert_eq!(
            lines,
            vec![
                Hunk { len: 5, id: 2 },
                Hunk { len: 1, id: 0 },
                Hunk { len: 2, id: 1 },
                Hunk { len: 9, id: 0 },
                Hunk { len: 4, id: 3 }
            ],
        );
    }

    #[test]
    fn lines_range_removal() {
        let mut lines: Lines<_> = vec![
            Hunk { len: 1, id: 0 }, // keep
            Hunk { len: 3, id: 1 }, // len: 1
            Hunk { len: 1, id: 0 }, // remove
            Hunk { len: 1, id: 0 }, // remove
            Hunk { len: 4, id: 4 }, // len: 1
            Hunk { len: 2, id: 3 }, // keep
        ]
        .into();

        lines.remove(2..7);

        let wanted = vec![
            Hunk { len: 1, id: 0 },
            Hunk { len: 1, id: 1 },
            Hunk { len: 3, id: 4 },
            Hunk { len: 2, id: 3 },
        ];

        assert_eq!(lines, wanted);
    }
}
