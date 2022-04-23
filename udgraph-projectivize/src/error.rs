use thiserror::Error;

/// Graph errors.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ProjectivizeError {
    /// The graph is missing relevant information.
    #[error("incomplete graph: {value:?}")]
    IncompleteGraph { value: String },
}
