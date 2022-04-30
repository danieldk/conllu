//! This crate provides (de)projectivizers for universal dependency graphs.
//!
//! A non-projective graph is a graph with crossing edges when the
//! words/vertices are laid out in sentence order. A projective graph
//! does not have crossing edges. This crate provides projectivizers,
//! which can rewrite non-projective graphs into a projective graphs.
//! The corresponding deprojectivizers can be used to reverse this
//! transformation.

mod error;
pub use error::Error;

mod graph_algo;
pub(crate) use crate::graph_algo::BfsWithDepth;

mod proj;
pub use proj::{Deprojectivize, HeadProjectivizer, Projectivize};

#[cfg(test)]
mod tests;
