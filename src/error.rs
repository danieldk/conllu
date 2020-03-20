use failure::Fail;

/// CoNLL-U parsing errors.
#[derive(Debug, Eq, Fail, PartialEq)]
pub enum ParseError {
    /// The form is missing in the CoNLL-U data.
    #[fail(display = "form field is missing")]
    MissingFormField,

    /// The feature field could not be parsed
    #[fail(display = "cannot parse feature field: {}", value)]
    IncorrectFeatureField { value: String },

    /// An integer field could not be parsed as an integer.
    #[fail(display = "cannot parse as integer field: {}", value)]
    ParseIntField { value: String },

    /// The identifier field could not be parsed.
    #[fail(display = "cannot parse as identifier field: {}", value)]
    ParseIdentifierField { value: String },
}

/// Graph errors.
#[derive(Debug, Fail)]
pub enum GraphError {
    /// The graph is missing relevant information.
    #[fail(display = "incomplete graph: {}", value)]
    IncompleteGraph { value: String },
}
