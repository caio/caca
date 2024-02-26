// A thin layer over gix::ObjectId that serializes
// as hex string instead of tagged enum + &[u8]
use urso::ObjectId;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash)]
pub struct HexId {
    pub id: urso::ObjectId,
}

impl std::fmt::Debug for HexId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id.to_hex())
    }
}

impl std::fmt::Display for HexId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.to_hex().fmt(f)
    }
}

impl From<ObjectId> for HexId {
    fn from(id: ObjectId) -> Self {
        Self { id }
    }
}

impl From<&ObjectId> for HexId {
    fn from(&id: &ObjectId) -> Self {
        Self { id }
    }
}

impl From<HexId> for ObjectId {
    fn from(val: HexId) -> Self {
        val.id
    }
}

impl serde::Serialize for HexId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // FIXME will break on sha256
        let mut hex = [0u8; 40];
        let max_len = self.id.hex_to_buf(hex.as_mut());
        let hex = std::str::from_utf8(&hex[..hex.len().min(max_len)]).expect("ascii only in hex");
        serializer.serialize_str(hex)
    }
}

#[cfg(test)]
mod tests {
    use serde_test::{assert_ser_tokens, Configure, Token};

    #[test]
    fn hexid_serializes_as_string() {
        let hex = super::HexId {
            id: urso::ObjectId::from_hex(&[b'0'; 40]).expect("null object hash is valid"),
        };
        assert_ser_tokens(
            &hex.compact(),
            &[Token::Str("0000000000000000000000000000000000000000")],
        );
    }
}
