use std::collections::HashMap;
use uuid::Uuid;

enum NodeKind {
    Sensor,
    Hidden,
    Output,
}

#[derive(Hash, Eq, PartialEq)]
struct NodeId(pub Uuid);

struct Node {
    node_id: NodeId,
    kind: NodeKind,
    bias: f32,
}

#[derive(Hash, Eq, PartialEq)]
struct EdgeId(pub Uuid);

struct Edge {
    edge_id: EdgeId,
    in_node_id: NodeId,
    out_node_id: NodeId,
    enabled: bool,
    weight: f32,
    innovation_number: i128,
}

#[derive(Hash, Eq, PartialEq)]
struct GenomeId(pub Uuid);
struct Genome<'a> {
    genome_id: GenomeId,
    nodes: HashMap<NodeId, &'a Node>,
    edges: HashMap<EdgeId, &'a Edge>,
}


// Species.
// Since the word "species" is both the signular and the plural form,
// it can cause some confusion, use the word "group" instead.
#[derive(Hash, Eq, PartialEq)]
struct GroupSn(pub i32);
struct Group<'a> {
    group_sn: GroupSn,
    genomes: HashMap<GenomeId, &'a Genome<'a>>,
}

struct GenerationSn(pub i32);
struct Generation<'a> {
    generation_sn: GenerationSn,
    groups: HashMap<GroupSn, &'a Group<'a>>
}


pub fn simulate(num_generations: i32) {
  println!("Number of generations to simulate: {}", num_generations);
}
