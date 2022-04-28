/// Graph errors.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    /// The graph is missing relevant information.
    #[error("incomplete graph: {value:?}")]
    IncompleteGraph { value: String },
}
