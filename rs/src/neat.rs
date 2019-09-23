use std::vec::Vec;
use std::collections::HashMap;
use uuid::Uuid;
use rand::{self, Rng, distributions::{Distribution, Uniform}};

#[derive(Eq, PartialEq)]
enum NodeKind {
    InputNode,
    HiddenNode,
    OutputNode,
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct NodeId(pub Uuid);

struct Node {
    node_id: NodeId,
    kind: NodeKind,
    bias: f32,
}

impl Node {
    /// Creates a new node with a random bias sampled from `bias_dist`
    fn new<D: Distribution<f32>, R: Rng>(kind: NodeKind, bias_dist: &D, rng: &mut R) -> Node {
        Node {
            node_id: NodeId(Uuid::new_v4()),
            kind: kind,
            bias: bias_dist.sample(rng)
        }
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct EdgeId(pub Uuid);

struct Edge {
    edge_id: EdgeId,
    in_node_id: NodeId,
    out_node_id: NodeId,
    enabled: bool,
    weight: f32,
    innovation_number: u128,
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct GenomeId(pub Uuid);

/// A `Genome` is a collection of `Node`s and `Edge`s
struct Genome {
    genome_id: GenomeId,
    nodes: HashMap<NodeId, Node>,
    edges: HashMap<EdgeId, Edge>,
}


// Species.
// Since the word "species" is both the signular and the plural form,
// it can cause some confusion, use the word "group" instead.
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct GroupSn(pub i32);

/// A `Group` is a collection of `Genome`s
struct Group {
    group_sn: GroupSn,
    genomes: HashMap<GenomeId, Genome>,
}


struct GenerationSn(pub i32);

/// A `Generation` is a collection of `Group`s
struct Generation {
    generation_sn: GenerationSn,
    groups: HashMap<GroupSn, Group>
}


struct InnovationNumberRegistry {
    // the next available innovation number
    next: u128,
    registry: HashMap<(NodeId, NodeId), u128>,
}

impl InnovationNumberRegistry {
    fn new() -> InnovationNumberRegistry {
        InnovationNumberRegistry {
            next: 0,
            registry: HashMap::new()
        }
    }

    fn get(&mut self, in_node_id: NodeId, out_node_id: NodeId) -> u128 {
        let key = (in_node_id, out_node_id);
        match self.registry.get(&key) {
            None => {
                let innov = self.next;
                self.next += 1;
                self.registry.insert(key, innov);
                innov
            }
            Some(x) => *x
        }
    }

}


fn create_initial_generation<D: Distribution<f32>, R: Rng>(
    num_genomes: i32, num_inputs: i32, num_outputs: i32,
    weight_dist: &D, bias_dist: &D, rng: &mut R
) -> Generation {
    let mut genomes = HashMap::new();
    let mut innovation_number_registry = InnovationNumberRegistry::new();
    for _ in 0..num_genomes {
        // first, create the nodes
        let mut nodes = HashMap::new();
        for _ in 0..num_inputs {
            let node = Node::new(NodeKind::InputNode, bias_dist, rng);
            assert!(nodes.insert(node.node_id, node).is_none());
        }
        for _ in 0..num_outputs {
            let node = Node::new(NodeKind::OutputNode, bias_dist, rng);
            assert!(nodes.insert(node.node_id, node).is_none());
        }

        // then, create the edges
        let mut edges = HashMap::new();
        let in_nodes = nodes.values().filter(|n| n.kind == NodeKind::InputNode);
        for in_node in in_nodes {
            let out_nodes = nodes.values().filter(|n| n.kind == NodeKind::OutputNode);
            for out_node in out_nodes {
                let edge = Edge {
                    edge_id: EdgeId(Uuid::new_v4()),
                    in_node_id: in_node.node_id,
                    out_node_id: out_node.node_id,
                    weight: weight_dist.sample(rng),
                    // TODO @incomplete: make this probability configurable
                    enabled: rng.gen_bool(0.9),
                    innovation_number: innovation_number_registry.get(in_node.node_id, out_node.node_id),
                };
                assert!(edges.insert(edge.edge_id, edge).is_none());
            }
        }

        // finally, put them together
        let genome = Genome {
            genome_id: GenomeId(Uuid::new_v4()),
            nodes: nodes,
            edges: edges};
        // println!("length of genome: {} nodes, {} edges", genome.nodes.len(), genome.edges.len());
        assert!(genomes.insert(genome.genome_id, genome).is_none());
    }

    let group0 = Group {
        group_sn: GroupSn(0),
        genomes: genomes
    };
    
    let mut groups = HashMap::new();
    groups.insert(group0.group_sn, group0);

    Generation {
        generation_sn: GenerationSn(0),
        groups: groups
    }
}

pub fn simulate(num_generations: u32) {
    let mut rng = rand::thread_rng();
    let weight_dist = Uniform::new(0.0, 1.0);
    let bias_dist = Uniform::new(0.0, 1.0);
    let gen0 = create_initial_generation(10, 3, 2, &weight_dist, &bias_dist, &mut rng);
}
