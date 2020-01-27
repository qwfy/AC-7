use std;
use std::vec::Vec;
use std::collections::HashMap;
use uuid::Uuid;
use rand::{self, Rng, distributions::{Distribution, Uniform}};
use rand::seq::SliceRandom;
use log::{info, trace, warn};

pub struct Param {
    pub num_generations: u32,
    pub compatability_threshold: f32
}


#[derive(Eq, PartialEq, Copy, Clone)]
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

type Nodes = HashMap<NodeId, Node>;
type Edges = HashMap<EdgeId, Edge>;

/// A `Genome` is a collection of `Node`s and `Edge`s
struct Genome {
    genome_id: GenomeId,
    nodes: Nodes,
    edges: Edges,
    fitness: f32,
}


// Species.
// Since the word "species" is both the signular and the plural form,
// it can cause some confusion, use the word "group" instead.
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct GroupSn(pub i32);

type Genomes = HashMap<GenomeId, Genome>;
/// A `Group` is a collection of `Genome`s
struct Group {
    group_sn: GroupSn,
    genomes: Genomes,
}


struct GenerationSn(pub u32);

impl GenerationSn {
    fn succ(&self) -> GenerationSn {
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

type Groups = HashMap<GroupSn, Group>;

/// A `Generation` is a collection of `Group`s
struct Generation {
    generation_sn: GenerationSn,
    groups: Groups
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
    let generation_sn = GenerationSn(0);
    info!("creating generation {}", generation_sn);
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
        // the input nodes are fully connected to the output nodes
        // TODO @incomplete: use sparse connection?
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
            edges: edges,
            // TODO @incomplete: calculate fitness
            fitness: 0.0
        };
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
        generation_sn: generation_sn,
        groups: groups
    }
}

pub fn simulate(param: &Param) {
    info!("simulation started");
    let mut rng = rand::thread_rng();
    let weight_dist = Uniform::new(0.0, 1.0);
    let bias_dist = Uniform::new(0.0, 1.0);

    let mut gen = create_initial_generation(10, 3, 2, &weight_dist, &bias_dist, &mut rng);
    for _ in 1..param.num_generations {
        gen = evolve(&gen, param);
        store_gen(&gen);
    }
}

// TODO @incomplete: this may or may not be stable
fn choose_representative(genomes: &Genomes) -> Option<&Genome>
{
    let mut all_genomes: Vec<&Genome> = genomes.values().collect();
    match all_genomes.get(0) {
        None => None,
        Some(x) => Some(x)
    }
}

fn speciation(groups: &Groups, compatability_threshold: f32)
    -> HashMap<GroupSn, Vec<&Genome>>
{

    // representatives of each group
    let mut representatives: HashMap<GroupSn, &Genome>
        = groups.iter().filter_map(
        |(group_sn, group)|
        match choose_representative(&group.genomes) {
            None => None,
            Some(genome) => Some((group_sn.clone(), genome))
        }
    ).collect();

    // initialize the new groups with the representative
    let mut new_groups: HashMap<GroupSn, Vec<&Genome>> = HashMap::new();
    for (group_sn, repre) in representatives.iter() {
        let genomes = vec![*repre];
        new_groups.insert(group_sn.clone(), genomes);
    }

    // iterate over each genome in the groups
    for group in groups.values() {
        for genome in group.genomes.values() {

            let mut processed = false;
            for (group_sn, repre) in representatives.iter() {
                let distance = calc_distance(genome, repre);
                if distance <= compatability_threshold {
                    new_groups.get_mut(&group_sn).unwrap().push(genome);
                    processed = true;
                    break
                }
            }
            if !processed {
                let next_group_sn = GroupSn(1 + representatives.keys().map(|x| x.0).max().unwrap());
                representatives.insert(next_group_sn, &genome);
                new_groups.insert(next_group_sn, vec![&genome]);
            }

        }
    }

    new_groups
}

fn calc_distance(x: &Genome, center: &Genome) -> f32 {
    // TODO @incomplete: implement this
    0.0
}

/// Creates a new generation from the old generation
fn evolve(old_gen: &Generation, param: &Param) -> Generation {
    info!("evolving generation {}", old_gen.generation_sn);

    let speciated = speciation(&old_gen.groups, param.compatability_threshold);

    let mut adjusted_fitnesses: HashMap<GroupSn, Vec<(GenomeId, f32)>> = unimplemented!();
    let group_sizes: HashMap<GroupSn, usize> = unimplemented!();

    let mut new_groups = HashMap::new();

    // create one group at a time
    for (group_sn, genomes) in speciated {
        let mut fitnesses = adjusted_fitnesses.get(&group_sn).unwrap();
        // TODO @incomplete: check for NaN in fitnesses
        fitnesses.sort_by(|(_, fit1), (_, fit2)| (-fit1).partial_cmp(&-fit2).unwrap());

        let pop_size = group_sizes.get(&group_sn).unwrap();
        let most_fit_ids: Vec<&GenomeId> = fitnesses.iter()
            .take(*pop_size)
            .map(|(genome_id, _fit)| genome_id)
            .collect();

        // new genomes of this group
        let mut new_genomes = HashMap::new();
        let mut rng = rand::thread_rng();
        for _ in 0..*pop_size {
            let m = most_fit_ids.choose(&mut rng).unwrap();
            let f = most_fit_ids.choose(&mut rng).unwrap();
            let m = find_genome_by_id(m, &old_gen.groups).unwrap();
            let f = find_genome_by_id(f, &old_gen.groups).unwrap();
            let child = mate(m, f);
            new_genomes.insert(child.genome_id, child);
        }

        new_groups.insert(group_sn, Group {
            group_sn: group_sn,
            genomes: new_genomes
        });
    }

    Generation {
        generation_sn: old_gen.generation_sn.succ(),
        groups: new_groups
    }
}


fn mate(m: &Genome, f: &Genome) -> Genome {
    unimplemented!();
}

fn store_gen(gen: &Generation) {
    info!("storing generation {}", gen.generation_sn);
    // TODO @incomplete: implement this
}

fn find_genome_by_id<'a>(genome_id: &GenomeId, groups: &'a Groups) -> Option<&'a Genome> {
    for group in groups.values() {
        for genome in group.genomes.values() {
            if &genome.genome_id == genome_id {
                return Some(genome)
            }
        }
    }
    return None
}
