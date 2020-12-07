use std::io;

use thiserror::Error;

/// CoNLL-U IO error.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum IOError {
    /// Error in file IO.
    #[error("error reading treebank")]
    IO(#[from] io::Error),

    /// CoNLL-U parsing error.
    #[error(transparent)]
    Parse(#[from] ParseError),
}

/// CoNLL-U parsing errors.
#[derive(Debug, Error, Eq, PartialEq)]
#[non_exhaustive]
pub enum ParseError {
    /// The form is missing in the CoNLL-U data.
    #[error("form field is missing")]
    MissingFormField,

    /// The feature field could not be parsed
    #[error("cannot parse feature field: {value:?}")]
    IncorrectFeatureField { value: String },

    /// An integer field could not be parsed as an integer.
    #[error("cannot parse as integer field: {value:?}")]
    ParseIntField { value: String },

    /// The identifier field could not be parsed.
    #[error("cannot parse as identifier field: {value:?})")]
    ParseIdentifierField { value: String },

    /// Dependency relation without a head.
    #[error("dependency relation without a head: {token:?}")]
    RelationWithoutHead { token: String },
}
