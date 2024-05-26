use bincode::Options;
use revision::Revisioned;
use std::io::{Read, Write};

use crate::SmolStr;

impl Revisioned for SmolStr {
    #[inline]
    fn revision() -> u16 {
        1
    }

    fn serialize_revisioned<W: Write>(&self, w: &mut W) -> Result<(), revision::Error> {
        bincode::options()
            .with_no_limit()
            .with_little_endian()
            .with_varint_encoding()
            .reject_trailing_bytes()
            .serialize_into(w, self)
            .map_err(|ref err| revision::Error::Serialize(format!("{:?}", err)))
    }

    fn deserialize_revisioned<R: Read>(r: &mut R) -> Result<Self, revision::Error>
    where
        Self: Sized,
    {
        bincode::options()
            .with_no_limit()
            .with_little_endian()
            .with_varint_encoding()
            .reject_trailing_bytes()
            .deserialize_from(r)
            .map_err(|ref err| revision::Error::Deserialize(format!("{:?}", err)))
    }
}
