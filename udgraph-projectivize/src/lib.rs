mod error;
pub use error::Error;

mod graph_algo;
pub(crate) use crate::graph_algo::BfsWithDepth;

mod proj;
pub use proj::{Deprojectivize, HeadProjectivizer, Projectivize};

#[cfg(test)]
mod tests;
