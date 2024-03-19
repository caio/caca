use std::ops::Range;

use gix::diff::blob::{diff as imara_diff, intern::InternedInput, Algorithm, Sink};

pub(crate) fn diff(before: &str, after: &str) -> super::UnifiedDiff {
    let input = InternedInput::new(before, after);
    let sink = StructuredSink::new(&input);

    imara_diff(Algorithm::Histogram, &input, sink)
}

pub(crate) fn similarity(before: &str, after: &str) -> f32 {
    let before_len = before.len();
    let after_len = after.len();
    debug_assert!(
        before_len > 0 || after_len > 0,
        "at least one of the sides must be non empty"
    );
    let input = InternedInput::new(before, after);
    let sink = RemovedBytes::new(&input);

    let removed_bytes = imara_diff(Algorithm::Myers, &input, sink);
    (before_len - removed_bytes) as f32 / before_len.max(after_len) as f32
}

struct RemovedBytes<'a> {
    removed_bytes: usize,
    input: &'a InternedInput<&'a str>,
}

impl<'a> RemovedBytes<'a> {
    fn new(input: &'a InternedInput<&'a str>) -> Self {
        Self {
            input,
            removed_bytes: 0,
        }
    }
}

// gix's Statistics sink
// https://github.com/Byron/gitoxide/blob/72274107fdb8c8faa93a4abbe1382ca3301003c9/gix/src/object/tree/diff/tracked.rs#L407
impl<'a> Sink for RemovedBytes<'a> {
    type Out = usize;

    fn process_change(&mut self, before: Range<u32>, _after: Range<u32>) {
        self.removed_bytes += self.input.before[before.start as usize..before.end as usize]
            .iter()
            .map(|token| self.input.interner[*token].len())
            .sum::<usize>();
    }

    fn finish(self) -> Self::Out {
        self.removed_bytes
    }
}

// imara-diff's unified diff sink, but written
// to structs instead of a std::fmt::Write
// https://github.com/pascalkuthe/imara-diff/blob/30736cc43f0aa63b340c26b48aa39b98a3930de7/src/unified_diff.rs
struct StructuredSink<'a> {
    chunks: Vec<super::Chunk>,

    input: &'a InternedInput<&'a str>,
    pos: u32,
    before_hunk_start: u32,
    before_hunk_len: u32,
    after_hunk_start: u32,
    after_hunk_len: u32,

    lines: Vec<super::Line>,
}

impl<'a> StructuredSink<'a> {
    fn new(input: &'a InternedInput<&'a str>) -> Self {
        Self {
            input,
            chunks: Vec::new(),
            pos: 0,
            before_hunk_start: 0,
            before_hunk_len: 0,
            after_hunk_start: 0,
            after_hunk_len: 0,
            lines: Vec::new(),
        }
    }

    fn register_removals(&mut self, range: Range<u32>) {
        self.before_hunk_len += range.end - range.start;
        let range = range.start as usize..range.end as usize;

        for &line in &self.input.before[range] {
            self.lines.push(super::Line::Removal(String::from(
                self.input.interner[line],
            )));
        }
    }

    fn register_additions(&mut self, range: Range<u32>) {
        self.after_hunk_len += range.end - range.start;
        for &line in &self.input.after[(range.start as usize)..(range.end as usize)] {
            self.lines.push(super::Line::Addition(String::from(
                self.input.interner[line],
            )));
        }
    }

    fn register_context(&mut self, range: Range<u32>) {
        for &line in &self.input.before[(range.start as usize)..(range.end as usize)] {
            self.lines.push(super::Line::Context(String::from(
                self.input.interner[line],
            )));
        }
    }

    fn flush(&mut self) {
        if self.before_hunk_len == 0 && self.after_hunk_len == 0 {
            return;
        }

        // Advance to collect the context _after_ the changes
        // being careful not to go beyond the original input length
        let end = (self.pos + 3).min(self.input.before.len() as u32);
        self.advance(end, end);

        let lines = std::mem::take(&mut self.lines);
        let before_pos = self.before_hunk_start..(self.before_hunk_start + self.before_hunk_len);
        let after_pos = self.after_hunk_start..(self.after_hunk_start + self.after_hunk_len);

        self.chunks.push(super::Chunk {
            before_pos,
            after_pos,
            lines,
        });

        self.before_hunk_len = 0;
        self.after_hunk_len = 0;
    }

    fn advance(&mut self, context_to: u32, pos_to: u32) {
        self.register_context(self.pos..context_to);
        let len = context_to - self.pos;
        self.pos = pos_to;
        self.before_hunk_len += len;
        self.after_hunk_len += len;
    }
}

impl<'a> Sink for StructuredSink<'a> {
    type Out = super::UnifiedDiff;

    fn process_change(&mut self, before: Range<u32>, after: Range<u32>) {
        if before.start - self.pos > 6 {
            self.flush();
            self.pos = before.start - 3;
            self.before_hunk_start = self.pos;
            self.after_hunk_start = after.start - 3;
        }
        self.advance(before.start, before.end);
        self.register_removals(before);
        self.register_additions(after);
    }

    fn finish(mut self) -> Self::Out {
        self.flush();

        super::UnifiedDiff {
            before_lineno: self.input.before.len(),
            after_lineno: self.input.after.len(),
            chunks: self.chunks,
        }
    }
}

#[cfg(test)]
mod tests {
    use gix::diff::blob::{
        diff as imara_diff, intern::InternedInput, Algorithm, UnifiedDiffBuilder,
    };

    fn assert_unified_diff_eq(before: &str, after: &str) {
        let input = InternedInput::new(before, after);
        let expected = imara_diff(Algorithm::Myers, &input, UnifiedDiffBuilder::new(&input));

        let ours = super::diff(before, after);
        let got = ours.as_patch().expect("formatting works");

        assert_eq!(
            got, expected,
            "patches don't match!\n\nbefore:\n{before}\n\nafter:\n{after}\n"
        );
    }

    #[test]
    fn trivial() {
        assert_unified_diff_eq("", "");
        assert_unified_diff_eq("", "a");
        assert_unified_diff_eq("a", "");
        assert_unified_diff_eq("a", "b");
    }

    #[test]
    fn multiple_chunks() {
        let before = "
1
2
FIXME
4
5
6
7
8
9
10
11
REMOVEME
12
13
";
        let after = "
1
2
3
4
5
6
7
8
9
10
11
12
13
";

        assert_unified_diff_eq(before, after);
    }

    #[test]
    fn unchanged_within_delta() {
        let before = "
1
2
3
4
5
6
7
8
9
10
11
12
13
14
";
        let after = "
1
2
3
4
5
6
THIS IS NEW
7
HERE TOO
8
9
10
11
12
13
14";

        assert_unified_diff_eq(before, after);
    }
}
