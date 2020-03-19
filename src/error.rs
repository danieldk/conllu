use failure::Fail;

/// CoNLL-X read errors.
#[derive(Debug, Fail)]
pub enum ReadError {
    /// The form is missing in the CoNLL-X data.
    #[fail(display = "form field is missing")]
    MissingFormField,

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
