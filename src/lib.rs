mod error;
pub use crate::error::{GraphError, ReadError};

pub mod graph;

mod graph_algo;
pub(crate) use crate::graph_algo::BfsWithDepth;

pub mod proj;

pub mod conllx;

#[cfg(test)]
mod tests;
