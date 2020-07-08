use std;
use std::collections::HashMap;
use std::iter::Sum;
use std::vec::Vec;

use log::{debug, info, trace, warn};
use ndarray::Array;
use ordered_map::OrderedMap;
use rand::{
    self,
    distributions::{Distribution, Uniform},
    Rng,
};
use rand::seq::SliceRandom;
use uuid::Uuid;
use std::panic::resume_unwind;

pub struct Param {
    pub num_generations: u32,
    pub compatibility_threshold: f64,
    pub c_excess: f64,
    pub c_disjoint: f64,
    pub c_common: f64,
    pub initial_population_size: u32,
    pub new_edge_enable_probability: f64,

    pub num_inputs: u32,
    pub num_outputs: u32,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum NodeKind {
    InputNode,
    HiddenNode,
    OutputNode,
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub struct NodeId(pub Uuid);

pub struct Node {
    pub node_id: NodeId,
    pub kind: NodeKind,
    pub bias: f64
}

impl Node {
    /// Creates a new node with a random bias sampled from `bias_dist`
    pub fn new<D: Distribution<f64>, R: Rng>(kind: NodeKind, bias_dist: &D, rng: &mut R) -> Node {
        Node {
            node_id: NodeId(Uuid::new_v4()),
            kind,
            bias: bias_dist.sample(rng)
        }
    }

    pub fn make_copy(&self) -> Node {
        Node {
            node_id: self.node_id.clone(),
            kind: self.kind.clone(),
            bias: self.bias.clone()
        }
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub struct EdgeId(pub Uuid);

pub struct Edge {
    pub edge_id: EdgeId,
    pub in_node_id: NodeId,
    pub out_node_id: NodeId,
    pub enabled: bool,
    pub weight: f64,
    pub innovation_number: u128,
}

impl Edge {
    pub fn make_copy(&self) -> Edge {
        Edge {
            edge_id: self.edge_id.clone(),
            in_node_id: self.in_node_id.clone(),
            out_node_id: self.out_node_id.clone(),
            enabled: self.enabled.clone(),
            weight: self.weight.clone(),
            innovation_number: self.innovation_number.clone(),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub struct GenomeId(pub Uuid);

pub type Nodes = HashMap<NodeId, Node>;
pub type Edges = OrderedMap<EdgeId, Edge, u128>;

/// A `Genome` is a collection of `Node`s and `Edge`s
pub struct Genome {
    pub genome_id: GenomeId,
    pub nodes: Nodes,
    pub edges: Edges,
    pub fitness: f64,
}

// Species.
// Since the word "species" is both the singular and the plural form,
// it can cause some confusion, use the word "group" instead.
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct GroupSn(pub i32);

pub type Genomes = HashMap<GenomeId, Genome>;

/// A `Group` is a collection of `Genome`s
pub struct Group {
    pub group_sn: GroupSn,
    pub genomes: Genomes,
}

pub struct GenerationSn(pub u32);

impl GenerationSn {
    pub fn succ(&self) -> GenerationSn {
        let GenerationSn(old) = self;
        GenerationSn(old + 1)
    }
}

impl std::fmt::Display for GenerationSn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let GenerationSn(sn) = self;
        write!(f, "GenerationSn({})", sn)
    }
}

pub type Groups = HashMap<GroupSn, Group>;

/// A `Generation` is a collection of `Group`s
pub struct Generation {
    pub generation_sn: GenerationSn,
    pub groups: Groups,
}

pub struct InnovationNumberRegistry {
    // the next available innovation number
    pub next: u128,
    pub registry: HashMap<(NodeId, NodeId), u128>,
}

impl InnovationNumberRegistry {
    pub fn new() -> InnovationNumberRegistry {
        InnovationNumberRegistry {
            next: 0,
            registry: HashMap::new(),
        }
    }

    pub fn get(&mut self, in_node_id: NodeId, out_node_id: NodeId) -> u128 {
        let key = (in_node_id, out_node_id);
        match self.registry.get(&key) {
            None => {
                let innov = self.next;
                self.next += 1;
                self.registry.insert(key, innov);
                innov
            }
            Some(x) => *x,
        }
    }
}
