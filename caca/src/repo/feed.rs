use std::{
    cmp::{Ordering, Reverse},
    collections::BinaryHeap,
    num::NonZeroUsize,
};

use super::{DateTime, HexId, Signature};

#[derive(Clone, Debug, PartialEq, Eq, std::hash::Hash, serde::Serialize)]
#[serde(tag = "kind")]
pub(crate) enum FeedEntry {
    Tag(TagActivity),
    Branch(BranchActivity),
}

// Tag <tag> created on commit <id> by <author> | <time>
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub(crate) struct TagActivity {
    // XXX need unique id for feed
    pub tag_name: String,
    pub browse_url: String,

    pub tagger: Option<Signature>,
    pub annotation: Option<String>,

    pub commit: CommitActivity,
}

impl std::hash::Hash for TagActivity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.browse_url.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub(crate) struct CommitActivity {
    pub author: Signature,

    pub id: HexId,
    pub url: String,

    pub title: String,
    pub body: String,
}

// <id> <commit message> on <branch> by <author> | <time>
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub(crate) struct BranchActivity {
    pub branch_name: String,
    pub browse_url: String,
    pub is_default_branch: bool,

    pub commit: CommitActivity,
}

impl std::hash::Hash for BranchActivity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.browse_url.hash(state);
    }
}

impl FeedEntry {
    pub fn time(&self) -> DateTime {
        match self {
            FeedEntry::Tag(t) => t.tagger.as_ref().map_or(t.commit.author.time, |t| t.time),
            FeedEntry::Branch(b) => b.commit.author.time,
        }
    }

    // used for sorting stability
    fn id(&self) -> HexId {
        match self {
            FeedEntry::Tag(t) => t.commit.id,
            FeedEntry::Branch(b) => b.commit.id,
        }
    }
}

impl PartialOrd for FeedEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FeedEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time().cmp(&other.time()).then(match (self, other) {
            (FeedEntry::Tag(a), FeedEntry::Tag(b)) => a.tag_name.cmp(&b.tag_name),
            (FeedEntry::Branch(a), FeedEntry::Branch(b)) => a.branch_name.cmp(&b.branch_name),
            // Tag > Branch
            (FeedEntry::Branch(_), FeedEntry::Tag(_)) => Ordering::Less,
            (FeedEntry::Tag(_), FeedEntry::Branch(_)) => Ordering::Greater,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub(crate) struct GlobalFeedEntry {
    pub(crate) repo: String,
    #[serde(flatten)]
    pub(crate) entry: FeedEntry,
}

impl PartialOrd for GlobalFeedEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GlobalFeedEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        // time > name > id
        self.entry.time().cmp(&other.entry.time()).then_with(|| {
            self.repo
                .cmp(&other.repo)
                .then_with(|| self.entry.id().cmp(&other.entry.id()))
        })
    }
}

#[derive(Debug)]
pub(crate) struct TopK<T: Ord> {
    q: BinaryHeap<Reverse<T>>,
    k: usize,
}

impl<T: Ord> TopK<T> {
    pub(crate) fn new(k: NonZeroUsize) -> Self {
        let k = k.get();
        let q = BinaryHeap::with_capacity(k);
        Self { q, k }
    }

    pub(crate) fn min(&self) -> Option<&T> {
        self.q.peek().map(|Reverse(i)| i)
    }

    pub(crate) fn len(&self) -> usize {
        self.q.len()
    }

    pub(crate) fn insert(&mut self, info: T) -> bool {
        if self.q.len() == self.k {
            if let Some(mut oldest) = self.q.peek_mut() {
                if oldest.0 < info {
                    *oldest = Reverse(info);
                    return true;
                }
            }
        } else {
            self.q.push(Reverse(info));
            return true;
        }
        false
    }

    pub(crate) fn finish(self) -> Vec<T> {
        self.q
            .into_sorted_vec()
            .into_iter()
            .map(|Reverse(c)| c)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn topk_works() {
        let mut q = TopK::new(NonZeroUsize::new(3).unwrap());
        for i in 0..6 {
            q.insert(i);
        }
        assert_eq!(vec![5, 4, 3], q.finish());
    }
}
