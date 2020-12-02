use thiserror::Error;

/// Graph errors.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum GraphError {
    /// The graph is missing relevant information.
    #[error("incomplete graph: {value:?}")]
    IncompleteGraph { value: String },
}
