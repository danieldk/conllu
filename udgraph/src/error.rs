use thiserror::Error;

/// Graph processing error.
#[derive(Debug, Error, Eq, PartialEq)]
#[non_exhaustive]
pub enum GraphError {
    #[error("dependent {dependent:?} is out of bounds for graph with {node_count:?} vertices")]
    DependentOutOfBounds { dependent: usize, node_count: usize },

    #[error("head {head:?} is out of bounds for graph with {node_count:?} vertices")]
    HeadOutOfBounds { head: usize, node_count: usize },
}
