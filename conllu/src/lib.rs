//! Traits to read/write `udgraph` graphs from CoNLL-U format.

mod error;
pub use crate::error::{Error, ParseError};

pub mod io;

pub mod display;

#[cfg(test)]
mod tests;
