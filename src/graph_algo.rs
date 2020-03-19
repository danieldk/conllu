use std::collections::VecDeque;
use std::mem;

pub use petgraph::visit::{GraphRef, IntoNeighbors, VisitMap, Visitable, Walker};

pub struct BfsWithDepth<N, VM> {
    cur_stack: VecDeque<N>,
    next_stack: VecDeque<N>,
    discovered: VM,
    depth: usize,
}

impl<N, VM> BfsWithDepth<N, VM>
where
    N: Copy + PartialEq,
    VM: VisitMap<N>,
{
    pub fn new<G>(graph: G, start: N) -> Self
    where
        G: GraphRef + Visitable<NodeId = N, Map = VM>,
    {
        let mut discovered = graph.visit_map();
        discovered.visit(start);
        let mut cur_stack = VecDeque::new();
        cur_stack.push_back(start);

        BfsWithDepth {
            cur_stack,
            next_stack: VecDeque::new(),
            discovered,
            depth: 0,
        }
    }

    pub fn next(&mut self, graph: impl IntoNeighbors<NodeId = N>) -> Option<(N, usize)> {
        if self.cur_stack.is_empty() && !self.next_stack.is_empty() {
            mem::swap(&mut self.cur_stack, &mut self.next_stack);
            self.depth += 1;
        }

        if let Some(node) = self.cur_stack.pop_front() {
            for succ in graph.neighbors(node) {
                if self.discovered.visit(succ) {
                    self.next_stack.push_back(succ)
                }
            }

            return Some((node, self.depth));
        }

        None
    }
}

impl<G> Walker<G> for BfsWithDepth<G::NodeId, G::Map>
where
    G: IntoNeighbors + Visitable,
{
    type Item = (G::NodeId, usize);

    fn walk_next(&mut self, context: G) -> Option<Self::Item> {
        self.next(context)
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use petgraph::graph::{node_index, Graph, NodeIndex};
    use petgraph::visit::Walker;

    use crate::BfsWithDepth;

    lazy_static! {
    static ref GRAPH_DEPTH_4: Graph<(), usize> = Graph::<(), usize>::from_edges(&[
        (0, 1), (0, 2), (0, 3),
        (1, 4), (1, 5), (5, 6),
        (3, 7), (3, 8)
    ]);

    /// Note: for firected graphs, neighbors are returned in reverse order
    /// of addition.
    static ref GRAPH_DEPTH_4_BFS_WITH_DEPTH: Vec<(NodeIndex, usize)> = vec![
        (node_index(0),0),
        (node_index(3),1),
        (node_index(2),1),
        (node_index(1),1),
        (node_index(8),2),
        (node_index(7),2),
        (node_index(5),2),
        (node_index(4),2),
        (node_index(6),3)];
    }

    #[test]
    pub fn bfs_with_depth_test() {
        let visits: Vec<_> = BfsWithDepth::new(&*GRAPH_DEPTH_4, node_index(0))
            .iter(&*GRAPH_DEPTH_4)
            .collect();
        assert_eq!(*GRAPH_DEPTH_4_BFS_WITH_DEPTH, visits);
    }

}
