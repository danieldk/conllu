//! Dependency graphs.

use std::borrow::Borrow;
use std::fmt::{self, Display, Formatter};
use std::iter::FromIterator;
use std::mem;
use std::ops::{Index, IndexMut};

use petgraph::graph::{node_index, DiGraph, NodeIndices, NodeWeightsMut};
use petgraph::visit::EdgeRef;
use petgraph::Direction;

use crate::error::GraphError;
use crate::token::Token;

/// Dependency graph node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Node {
    /// Root node.
    Root,

    /// Token node.
    Token(Token),
}

impl Node {
    pub fn is_root(&self) -> bool {
        !self.is_token()
    }

    pub fn is_token(&self) -> bool {
        match self {
            Node::Root => false,
            Node::Token(_) => true,
        }
    }

    pub fn token(&self) -> Option<&Token> {
        match self {
            Node::Root => None,
            Node::Token(token) => Some(token),
        }
    }

    pub fn token_mut(&mut self) -> Option<&mut Token> {
        match self {
            Node::Root => None,
            Node::Token(token) => Some(token),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Sentence comment.
pub enum Comment {
    /// Attribute-value pair
    AttrVal { attr: String, val: String },

    /// String comment
    String(String),
}

impl Comment {
    /// Returns `true` if the comment is an attribute-value pair.
    pub fn is_attr_val(&self) -> bool {
        !self.is_string()
    }

    /// Returns `true` if the comment is a string.
    pub fn is_string(&self) -> bool {
        match self {
            Comment::String(_) => true,
            Comment::AttrVal { .. } => false,
        }
    }

    /// Get the comment attribute value pair.
    pub fn attr_val(&self) -> Option<(&str, &str)> {
        match self {
            Comment::AttrVal { attr, val } => Some((attr, val)),
            Comment::String(_) => None,
        }
    }

    /// Get the comment string.
    pub fn string(&self) -> Option<&str> {
        match self {
            Comment::AttrVal { .. } => None,
            Comment::String(val) => Some(val),
        }
    }
}

impl Display for Comment {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            Comment::AttrVal { attr, val } => write!(fmt, "# {} = {}", attr, val),
            Comment::String(val) => write!(fmt, "# {}", val),
        }
    }
}

/// A dependency triple.
///
/// A dependency triple consists of: a head index; a dependent index; and
/// an optional dependency label.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct DepTriple<S> {
    head: usize,
    dependent: usize,
    relation: Option<S>,
}

impl<S> DepTriple<S> {
    /// Construct a new dependency triple.
    pub fn new(head: usize, relation: Option<S>, dependent: usize) -> Self {
        DepTriple {
            head,
            dependent,
            relation,
        }
    }

    /// Get the dependent.
    pub fn dependent(&self) -> usize {
        self.dependent
    }

    /// Get the head.
    pub fn head(&self) -> usize {
        self.head
    }
}

impl<S> DepTriple<S>
where
    S: Borrow<str>,
{
    pub fn relation(&self) -> Option<&str> {
        self.relation.as_ref().map(Borrow::borrow)
    }
}

/// Relation type.
///
/// This enum is used in the underlying `petgraph` graph to
/// distinguish between universal dependencies and enhanced universal
/// dependencies.  This enum is public because the underlying
/// `DiGraph` can be retrieved using the `get_ref` and `into_inner`
/// methods of `Sentence`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RelationType {
    Regular,
    Enhanced,
}

/// Dependency edge.
pub type Edge = (RelationType, Option<String>);

/// A CoNLL-U dependency graph.
///
/// `Sentence` stores a dependency graph. The nodes in the graph
/// (except the special root node) are tokens that have the fields
/// of the CoNLL-U format. Dependency relations are stored as edges
/// in the graph.
///
/// This data structure is a thin wrapper around the `petgraph`
/// `DiGraph` data structure that enforces variants such as
/// single-headedness. The `into_inner`/`get_ref` methods can
/// be used to unwrap or get a reference to the wrapped graph.
#[derive(Clone, Debug)]
pub struct Sentence {
    comments: Vec<Comment>,
    graph: DiGraph<Node, Edge>,
}

#[allow(clippy::len_without_is_empty)]
impl Sentence {
    /// Construct a new sentence.
    ///
    /// The sentence will be constructed such that the first token is
    /// the root of the dependency graph:
    ///
    /// ```
    /// use udgraph::graph::{Node, Sentence};
    ///
    /// let sentence = Sentence::new();
    /// assert_eq!(sentence[0], Node::Root);
    /// ```
    pub fn new() -> Self {
        let mut graph = DiGraph::new();
        graph.add_node(Node::Root);
        Sentence {
            comments: Vec::new(),
            graph,
        }
    }

    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    pub fn comments_mut(&mut self) -> &mut Vec<Comment> {
        &mut self.comments
    }

    /// Get a reference to the `DiGraph` of the sentence.
    pub fn get_ref(&self) -> &DiGraph<Node, Edge> {
        &self.graph
    }

    /// Unwrap the `DiGraph` of the sentence.
    pub fn into_inner(self) -> DiGraph<Node, Edge> {
        self.graph
    }

    /// Get an iterator over the nodes in the graph.
    pub fn iter(&self) -> Iter {
        Iter {
            inner: self.graph.node_indices(),
            graph: &self.graph,
        }
    }

    /// Get a mutable iterator over the nodes in the graph.
    pub fn iter_mut(&mut self) -> IterMut {
        IterMut(self.graph.node_weights_mut())
    }

    /// Add a new token to the graph.
    ///
    /// Tokens should always be pushed in sentence order.
    ///
    /// Returns the index of the token. The first pushed token has index 1,
    /// since index 0 is reserved by the root of the graph.
    pub fn push(&mut self, token: Token) -> usize {
        self.graph.add_node(Node::Token(token)).index()
    }

    /// Get the dependency graph.
    pub fn dep_graph(&self) -> DepGraph {
        DepGraph {
            inner: &self.graph,
            relation_type: RelationType::Regular,
        }
    }

    /// Get the graph mutably.
    pub fn dep_graph_mut(&mut self) -> DepGraphMut {
        DepGraphMut {
            inner: &mut self.graph,
            relation_type: RelationType::Regular,
        }
    }

    /// Get the number of nodes in the dependency graph.
    ///
    /// This is equal to the number of tokens, plus one root node.
    pub fn len(&self) -> usize {
        self.graph.node_count()
    }

    /// Replace the comments by the given comments.
    ///
    /// Returns the old comments that are replaced.
    pub fn set_comments(&mut self, comments: impl Into<Vec<Comment>>) {
        let _ = mem::replace(&mut self.comments, comments.into());
    }
}

impl Default for Sentence {
    fn default() -> Self {
        Sentence::new()
    }
}

impl FromIterator<Token> for Sentence {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Token>,
    {
        let mut sentence = Sentence::new();
        for token in iter {
            sentence.push(token);
        }
        sentence
    }
}

/// Iterator over the nodes in a dependency graph.
pub struct Iter<'a> {
    inner: NodeIndices,
    graph: &'a DiGraph<Node, Edge>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|idx| &self.graph[idx])
    }
}

impl<'a> IntoIterator for &'a Sentence {
    type Item = &'a Node;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Mutable iterator over the nodes in a dependency graph.
pub struct IterMut<'a>(NodeWeightsMut<'a, Node>);

impl<'a> Iterator for IterMut<'a> {
    type Item = &'a mut Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> IntoIterator for &'a mut Sentence {
    type Item = &'a mut Node;
    type IntoIter = IterMut<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl Eq for Sentence {}

impl From<Sentence> for DiGraph<Node, Edge> {
    fn from(sentence: Sentence) -> Self {
        sentence.into_inner()
    }
}

impl<'a> From<&'a Sentence> for &'a DiGraph<Node, Edge> {
    fn from(sentence: &'a Sentence) -> Self {
        sentence.get_ref()
    }
}

impl Index<usize> for Sentence {
    type Output = Node;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.graph[node_index(idx)]
    }
}

impl IndexMut<usize> for Sentence {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.graph[node_index(idx)]
    }
}

impl PartialEq for Sentence {
    fn eq(&self, other: &Self) -> bool {
        self.comments == other.comments && self.dep_graph() == other.dep_graph()
    }
}

/// A graph view.
///
/// This data structure provides a view of a CoNLL-U dependency graph. The
/// view can be used to retrieve the dependents of a head or the head of a
/// dependent.
pub struct DepGraph<'a> {
    inner: &'a DiGraph<Node, Edge>,
    relation_type: RelationType,
}

#[allow(clippy::len_without_is_empty)]
impl<'a> DepGraph<'a> {
    /// Return an iterator over the dependents of `head`.
    pub fn dependents(&self, head: usize) -> impl Iterator<Item = DepTriple<&'a str>> {
        dependents_impl(self.inner, self.relation_type, head)
    }

    /// Return the head relation of `dependent`, if any.
    pub fn head(&self, dependent: usize) -> Option<DepTriple<&'a str>> {
        head_impl(self.inner, self.relation_type, dependent)
    }

    /// Get the number of nodes in the dependency graph.
    ///
    /// This is equal to the number of tokens, plus one root node.
    pub fn len(&self) -> usize {
        self.inner.node_count()
    }
}

impl<'a> Eq for DepGraph<'a> {}

impl<'a> Index<usize> for DepGraph<'a> {
    type Output = Node;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.inner[node_index(idx)]
    }
}

impl<'a, 'b> PartialEq<DepGraph<'b>> for DepGraph<'a> {
    fn eq(&self, other: &DepGraph<'b>) -> bool {
        // Cheap checks
        if self.inner.node_count() != other.inner.node_count()
            || self.inner.edge_count() != other.inner.edge_count()
        {
            return false;
        }

        for i in 0..self.len() {
            // Nodes should be equal.
            if self[i] != other[i] {
                return false;
            }

            // Relation to a token's head should be the same.
            if self.head(i) != other.head(i) {
                return false;
            }
        }

        true
    }
}

/// A mutable graph view.
///
/// This data structure provides a mutable view of a CoNLL-U dependency
/// graph. The view can be used to retrieve the dependents of a head or
/// the head of a dependent. In addition, the `add_deprel` method can be
/// used to add dependency relations to the graph.
pub struct DepGraphMut<'a> {
    inner: &'a mut DiGraph<Node, Edge>,
    relation_type: RelationType,
}

#[allow(clippy::len_without_is_empty)]
impl<'a> DepGraphMut<'a> {
    /// Add a dependency relation between `head` and `dependent`.
    ///
    /// If `dependent` already has a head relation, this relation is removed
    /// to ensure single-headedness.
    pub fn add_deprel<S>(&mut self, triple: DepTriple<S>) -> Result<(), GraphError>
    where
        S: Into<String>,
    {
        if triple.head() >= self.inner.node_count() {
            return Err(GraphError::HeadOutOfBounds {
                head: triple.head(),
                node_count: self.inner.node_count(),
            });
        }

        if triple.dependent() >= self.inner.node_count() {
            return Err(GraphError::DependentOutOfBounds {
                dependent: triple.head(),
                node_count: self.inner.node_count(),
            });
        }

        // Remove existing head relation (when present).
        if let Some(id) = self
            .inner
            .edges_directed(node_index(triple.dependent), Direction::Incoming)
            .filter(|e| e.weight().0 == self.relation_type)
            .map(|e| e.id())
            .next()
        {
            self.inner.remove_edge(id);
        }

        self.inner.add_edge(
            node_index(triple.head),
            node_index(triple.dependent),
            (self.relation_type, triple.relation.map(Into::into)),
        );

        Ok(())
    }

    /// Return an iterator over the dependents of `head`.
    pub fn dependents(&self, head: usize) -> impl Iterator<Item = DepTriple<&str>> {
        dependents_impl(self.inner, self.relation_type, head)
    }

    /// Return the head relation of `dependent`, if any.
    pub fn head(&self, dependent: usize) -> Option<DepTriple<&str>> {
        head_impl(self.inner, self.relation_type, dependent)
    }

    /// Remove relation of a token to its head.
    ///
    /// Returns the index of the head iff a head was removed.
    pub fn remove_head_rel(&mut self, dependent: usize) -> Option<DepTriple<String>> {
        // match instead of map to avoid simultaneous mutable and
        // immutable borrow.
        match self
            .inner
            .edges_directed(node_index(dependent), Direction::Incoming)
            .find(|e| e.weight().0 == self.relation_type)
        {
            Some(edge) => {
                let head = edge.source().index();
                let edge_id = edge.id();
                let weight = self.inner.remove_edge(edge_id);
                Some(DepTriple::new(head, weight.unwrap().1, dependent))
            }
            None => None,
        }
    }

    /// Get the number of nodes in the dependency graph.
    ///
    /// This is equal to the number of tokens, plus one root node.
    pub fn len(&self) -> usize {
        self.inner.node_count()
    }
}

impl<'a> Index<usize> for DepGraphMut<'a> {
    type Output = Node;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.inner[node_index(idx)]
    }
}

impl<'a> IndexMut<usize> for DepGraphMut<'a> {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.inner[node_index(idx)]
    }
}

fn dependents_impl(
    graph: &DiGraph<Node, Edge>,
    relation_type: RelationType,
    head: usize,
) -> impl Iterator<Item = DepTriple<&str>> {
    graph
        .edges_directed(node_index(head), Direction::Outgoing)
        .filter(move |e| e.weight().0 == relation_type)
        .map(|e| {
            DepTriple::new(
                e.source().index(),
                e.weight().1.as_deref(),
                e.target().index(),
            )
        })
}

fn head_impl(
    graph: &DiGraph<Node, Edge>,
    relation_type: RelationType,
    dependent: usize,
) -> Option<DepTriple<&str>> {
    graph
        .edges_directed(node_index(dependent), Direction::Incoming)
        .find(|e| e.weight().0 == relation_type)
        .map(|e| {
            DepTriple::new(
                e.source().index(),
                e.weight().1.as_deref(),
                e.target().index(),
            )
        })
}

#[cfg(test)]
mod tests {
    use super::{DepTriple, Node, Sentence, Token};

    #[test]
    fn add_deprel() {
        let mut g = Sentence::default();
        g.push(Token::new("Daniël"));
        g.push(Token::new("test"));
        g.push(Token::new("dit"));
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("wrong"), 1))
            .unwrap();
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("root"), 2))
            .unwrap();

        assert!(g.dep_graph().head(0).is_none());
        assert_eq!(
            g.dep_graph().head(1),
            Some(DepTriple::new(0, Some("wrong"), 1))
        );
        assert_eq!(
            g.dep_graph().head(2),
            Some(DepTriple::new(0, Some("root"), 2))
        );
        assert!(g.dep_graph().head(3).is_none());

        g.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("subj"), 1))
            .unwrap();
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("obj1"), 3))
            .unwrap();
        assert_eq!(
            g.dep_graph().head(1),
            Some(DepTriple::new(2, Some("subj"), 1))
        );
        assert_eq!(
            g.dep_graph().head(3),
            Some(DepTriple::new(2, Some("obj1"), 3))
        );
    }

    #[test]
    fn dependents() {
        let mut g = Sentence::default();
        g.push(Token::new("Daniël"));
        g.push(Token::new("test"));
        g.push(Token::new("dit"));
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("root"), 2))
            .unwrap();
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("subj"), 1))
            .unwrap();
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("obj1"), 3))
            .unwrap();

        let deps = g.dep_graph().dependents(0).collect::<Vec<_>>();
        assert_eq!(&deps, &[DepTriple::new(0, Some("root"), 2)]);

        assert!(g.dep_graph().dependents(1).next().is_none());

        let mut deps = g.dep_graph().dependents(2).collect::<Vec<_>>();
        deps.sort();
        assert_eq!(
            &deps,
            &[
                DepTriple::new(2, Some("subj"), 1),
                DepTriple::new(2, Some("obj1"), 3),
            ]
        );

        assert!(g.dep_graph().dependents(3).next().is_none());
    }

    #[test]
    fn equality() {
        let mut g1 = Sentence::default();
        g1.push(Token::new("does"));
        g1.push(Token::new("equality"));
        g1.push(Token::new("work"));

        let g2 = g1.clone();
        assert_eq!(g1, g2);

        g1.push(Token::new("?"));
        assert_ne!(g1, g2);

        let mut g3 = g1.clone();
        g1.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("root"), 3))
            .unwrap();
        g1.dep_graph_mut()
            .add_deprel(DepTriple::new(3, Some("subj"), 1))
            .unwrap();
        assert_ne!(g1, g3);
        g3.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("root"), 3))
            .unwrap();
        g3.dep_graph_mut()
            .add_deprel(DepTriple::new(3, Some("subj"), 1))
            .unwrap();
        assert_eq!(g1, g3);
        g3.dep_graph_mut()
            .add_deprel(DepTriple::new(3, Some("foobar"), 1))
            .unwrap();
        assert_ne!(g1, g3);

        let mut g4 = g1.clone();
        if let Node::Token(ref mut token) = g4[3] {
            token.set_xpos(Some("verb"));
        }
        assert_ne!(g1, g4);
    }

    #[test]
    #[should_panic(expected = "HeadOutOfBounds")]
    fn incorrect_head_is_rejected() {
        let mut g = Sentence::default();
        g.push(Token::new("Daniël"));
        g.push(Token::new("test"));
        g.push(Token::new("dit"));
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(4, Some("test"), 3))
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "DependentOutOfBounds")]
    fn incorrect_dependent_is_rejected() {
        let mut g = Sentence::default();
        g.push(Token::new("Daniël"));
        g.push(Token::new("test"));
        g.push(Token::new("dit"));
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(3, Some("test"), 4))
            .unwrap();
    }

    #[test]
    fn remove_deprel() {
        let mut g = Sentence::default();
        g.push(Token::new("Daniël"));
        g.push(Token::new("test"));
        g.push(Token::new("dit"));
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("wrong"), 1))
            .unwrap();
        g.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("root"), 2))
            .unwrap();
        assert_eq!(
            g.dep_graph_mut().remove_head_rel(1),
            Some(DepTriple::new(0, Some("wrong".to_owned()), 1))
        );
        assert!(g.dep_graph_mut().remove_head_rel(0).is_none());

        assert!(g.dep_graph().head(0).is_none());
        assert!(g.dep_graph().head(1).is_none());
        assert_eq!(
            g.dep_graph().head(2),
            Some(DepTriple::new(0, Some("root"), 2))
        );
    }
}
