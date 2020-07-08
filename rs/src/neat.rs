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

use crate::data::*;

fn create_initial_generation<D: Distribution<f64>, R: Rng>(
    param: &Param,
    innovation_number_registry: &mut InnovationNumberRegistry,
    weight_dist: &D,
    bias_dist: &D,
    rng: &mut R,
) -> Generation {
    let generation_sn = GenerationSn(0);
    info!("creating generation {}", generation_sn);

    // create the genomes of the single initial species
    let mut genomes = HashMap::new();
    for _ in 0..param.initial_population_size {
        // first, create the nodes
        let mut nodes = HashMap::new();
        for _ in 0..param.num_inputs {
            let node = Node::new(NodeKind::InputNode, bias_dist, rng);
            nodes.insert(node.node_id, node);
        }
        for _ in 0..param.num_outputs {
            let node = Node::new(NodeKind::OutputNode, bias_dist, rng);
            nodes.insert(node.node_id, node);
        }

        // then, create the edges
        let mut edges = OrderedMap::new(|e: &Edge| e.innovation_number);
        let in_nodes = nodes.values().filter(|n| n.kind == NodeKind::InputNode);
        let out_nodes = nodes.values().filter(|n| n.kind == NodeKind::OutputNode);

        // TODO FEATURE @incomplete: use sparse connection?
        // the input nodes are fully connected to the output nodes
        for in_node in in_nodes {
            for out_node in out_nodes {
                let edge = Edge {
                    edge_id: EdgeId(Uuid::new_v4()),
                    in_node_id: in_node.node_id,
                    out_node_id: out_node.node_id,
                    weight: weight_dist.sample(rng),
                    enabled: rng.gen_bool(param.new_edge_enable_probability),
                    innovation_number:
                        innovation_number_registry
                            .get(in_node.node_id, out_node.node_id),
                };
                edges.insert(edge.edge_id, edge);
            }
        }

        // finally, put them together
        let fitness = calculate_fitness(&nodes, &edges);
        let genome = Genome {
            genome_id: GenomeId(Uuid::new_v4()),
            nodes,
            edges,
            fitness,
        };
        genomes.insert(genome.genome_id, genome);
    }

    let group0 = Group {
        group_sn: GroupSn(0),
        genomes,
    };
    let mut groups = HashMap::new();
    groups.insert(group0.group_sn, group0);
    Generation {
        generation_sn,
        groups,
    }
}

/// Run one simulation
pub fn simulate(param: &Param) {
    let start_time = chrono::Local::now();
    info!("simulation started at {}", start_time);

    let mut rng = rand::thread_rng();
    let weight_dist = Uniform::new(0.0, 1.0);
    let bias_dist = Uniform::new(0.0, 1.0);
    let mut innovation_number_registry = InnovationNumberRegistry::new();

    let mut gen = create_initial_generation(
        param, &mut innovation_number_registry, &weight_dist, &bias_dist, &mut rng);
    for _ in 1..param.num_generations {
        gen = evolve(&gen, param);
        store_gen(&gen);
    }

    let stop_time = chrono::Local::now();
    info!("simulation ended at {}", stop_time);
    info!("simulation time cost {}", stop_time - start_time);
}

// TODO @incomplete: this may or may not be stable
fn choose_representative(genomes: &Genomes) -> Option<&Genome> {
    let mut all_genomes: Vec<&Genome> = genomes.values().collect();
    match all_genomes.get(0) {
        None => None,
        Some(x) => Some(x),
    }
}

fn speciate<'a, 'b>(groups: &'a Groups, param: &'b Param) -> HashMap<GroupSn, Vec<&'a Genome>> {
    // representatives of each group
    let mut representatives: HashMap<GroupSn, &Genome> = groups
        .iter()
        .filter_map(
            |(group_sn, group)| match choose_representative(&group.genomes) {
                None => None,
                Some(genome) => Some((group_sn.clone(), genome)),
            },
        )
        .collect();

    // initialize the new groups with the representative
    let mut new_groups: HashMap<GroupSn, Vec<&Genome>> = HashMap::new();
    for (group_sn, repre) in representatives.iter() {
        let genomes = vec![*repre];
        new_groups.insert(group_sn.clone(), genomes);
    }

    // iterate over each genome in the groups
    for group in groups.values() {
        info!("processing old group: {}, #genomes: {}", group.group_sn.0, group.genomes.len());
        for genome in group.genomes.values() {
            let mut processed = false;
            for (group_sn, repre) in representatives.iter() {
                let compatibility = calc_compatibility(genome, repre, param);
                if compatibility <= param.compatibility_threshold {
                    new_groups.get_mut(&group_sn).unwrap().push(genome);
                    processed = true;
                    break;
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


fn calc_compatibility(x: &Genome, y: &Genome, param: &Param) -> f64 {
    let xs_len = x.edges.len();
    let ys_len = y.edges.len();
    let mut xs = x.edges.descending_values().rev();
    let mut ys = y.edges.descending_values().rev();

    // n, the number of genes in the larger genome, normalizes for genome size
    // (can be set to 1 if both genomes are small, i.e. consist of fewer than 20 genes).
    let n = {
        let n1 = xs_len.max(ys_len);
        if n1 < 20 {
            1
        } else {
            n1
        }
    };

    let mut disjoint: usize = 0;
    let mut excess: usize = 0;
    let mut match_w = vec![];

    let mut x = xs.next();
    let mut y = ys.next();
    let mut xs_consumed: usize = 0;
    let mut ys_consumed: usize = 0;
    if x.is_some() { xs_consumed += 1; }
    if y.is_some() { ys_consumed += 1; }
    loop {
        match (x, y) {
            (None, None) => {
                // both consumed, calculate the compatibility
                let avg_w = match Array::from(match_w).mean() {
                    None => 0.0,
                    Some(x) => x
                };
                // FIXME: as f64 may truncate
                let compat = (param.c_excess * excess as f64 + param.c_disjoint * disjoint as f64) / n as f64 + param.c_common * avg_w;
                return compat;
            }
            (Some(_), None) => {
                excess += xs_len - xs_consumed;
                x = None;
            }
            (None, Some(_)) => {
                excess += ys_len - ys_consumed;
                y = None;
            }
            (Some(Edge { innovation_number: innov_x, weight: weight_x, .. }),
             Some(Edge { innovation_number: innov_y, weight: weight_y, .. })) => {
                if innov_x == innov_y {
                    match_w.push((weight_x + weight_y) / 2.0);
                    x = xs.next();
                    y = ys.next();
                    if x.is_some() { xs_consumed += 1; }
                    if y.is_none() { ys_consumed += 1; };
                } else if innov_x < innov_y {
                    disjoint += 1;
                    x = xs.next();
                    if x.is_some() { xs_consumed += 1; }
                } else if innov_y < innov_x {
                    disjoint += 1;
                    y = ys.next();
                    if y.is_none() { ys_consumed += 1; };
                } else {
                    assert!(false)
                }
            }
        }
    }
}

/// Creates a new generation from the old generation
fn evolve(old_gen: &Generation, param: &Param) -> Generation {
    info!("evolving generation {}", old_gen.generation_sn);

    let speciated = speciate(&old_gen.groups, &param);
    info!("#species: {}", speciated.len());

    let fits: HashMap<GroupSn, Vec<(GenomeId, f64)>> = speciated.iter()
        .map(|(group_sn, genomes)| {
            // FIXME: the cast is unsafe
            let size = genomes.len() as f64;
            let adjusted: Vec<(GenomeId, f64)> = genomes.iter()
                .map(|&g| (g.genome_id.clone(), g.fitness / size))
                .collect();
            (*group_sn, adjusted)
        })
        .collect();

    let group_sizes: HashMap<GroupSn, usize> = {
        let summed_by_group: Vec<(GroupSn, f64)> = fits.iter()
            .map(|(group_sn, id_fits)| {
                let sum: Vec<f64> = id_fits.iter().map(|(_, fit)| *fit).collect();
                let sum = Array::from(sum).sum();
                (*group_sn, sum)
            })
            .collect();
        debug!("summed_by_group: {:?}", summed_by_group);

        let total_sum: f64 = std::iter::Sum::sum(
            summed_by_group.iter()
                .map(|(_, s)| *s));
        debug!("total sum: {}", total_sum);
        assert_ne!(total_sum, 0.0);

        summed_by_group.iter()
            .map(|(group_sn, sum)| (*group_sn, f64::ceil(sum / total_sum) as usize))
            .collect()
    };
    debug!("group_sizes: {:?}", group_sizes);

    let mut new_groups = HashMap::new();

    // create one group at a time
    for (group_sn, genomes) in speciated {
        info!("creating group {}", group_sn.0);
        let mut fits = fits[&group_sn].clone();
        // TODO @incomplete: check for NaN in fits
        fits.sort_by(|(_, fit1), (_, fit2)| (-fit1).partial_cmp(&-fit2).unwrap());

        let pop_size = group_sizes[&group_sn];
        info!("pop size: {}", pop_size);
        let most_fit_ids: Vec<&GenomeId> = fits
            .iter()
            .take(pop_size)
            .map(|(genome_id, _fit)| genome_id)
            .collect();

        // new genomes of this group
        let mut new_genomes = HashMap::new();
        let mut rng = rand::thread_rng();
        for i in 0..pop_size {
            let m = most_fit_ids.choose(&mut rng).unwrap();
            let f = most_fit_ids.choose(&mut rng).unwrap();
            let m = find_genome_by_id(m, &old_gen.groups).unwrap();
            let f = find_genome_by_id(f, &old_gen.groups).unwrap();
            let child = mate(m, f);
            new_genomes.insert(child.genome_id, child);
        }

        new_groups.insert(
            group_sn,
            Group {
                group_sn,
                genomes: new_genomes,
            },
        );
    }

    Generation {
        generation_sn: old_gen.generation_sn.succ(),
        groups: new_groups,
    }
}



fn mate(x: &Genome, y: &Genome) -> Genome
{
    // if there is a winner, it's a
    let (a, b, is_tie) =
        if x.fitness > y.fitness { (x, y, false) }
        else if x.fitness < y.fitness { (y, x, false) }
        else { (x, y, true) };

    let mut nodes = HashMap::new();
    let mut edges = OrderedMap::new(|e: &Edge| e.innovation_number);

    let mut a_edges = a.edges.descending_values().rev();
    let mut b_edges = b.edges.descending_values().rev();
    let mut a_edge = a_edges.next();
    let mut b_edge = b_edges.next();

    let insert_edge = |ns: &mut Nodes, es: &mut Edges, all_nodes: &Nodes, e: &Edge|
        {
            ns.insert(e.in_node_id, all_nodes[&e.in_node_id].make_copy());
            ns.insert(e.out_node_id, all_nodes[&e.out_node_id].make_copy());
            es.insert(e.edge_id, e.make_copy());
        };

    loop {
        match (a_edge, b_edge) {
            (None, None) => {
                // both exhausted, create the new genome
                let fitness = calculate_fitness(&nodes, &edges);
                return Genome {
                    genome_id: GenomeId(Uuid::new_v4()),
                    nodes,
                    edges,
                    fitness
                }
            },
            (Some(ae), None) => {
                // no matter win or tie, insert ae
                insert_edge(&mut nodes, &mut edges, &a.nodes, ae);
                a_edge = a_edges.next();
            },
            (None, Some(be)) => {
                // insert be only on tie
                if is_tie {
                    insert_edge(&mut nodes, &mut edges, &b.nodes, be)
                }
                b_edge = b_edges.next();
            },
            (Some(ae), Some(be)) => {
                if ae.innovation_number == be.innovation_number {
                    // insert ae no matter when or tie
                    // insert be only on tie
                    insert_edge(&mut nodes, &mut edges, &a.nodes, ae);
                    if is_tie {
                        insert_edge(&mut nodes, &mut edges, &b.nodes, be);
                    }
                    // advance both, since innovation number is the same
                    a_edge = a_edges.next();
                    b_edge = b_edges.next();
                } else if ae.innovation_number < be.innovation_number {
                    // insert ae, since it is winner's disjoint
                    insert_edge(&mut nodes, &mut edges, &a.nodes, ae);
                    // b is not advanced, waiting for a to catch up
                    a_edge = a_edges.next();
                } else if be.innovation_number < ae.innovation_number {
                    // insert be only on tie, since it is the loser's disjoint
                    if is_tie {
                        insert_edge(&mut nodes, &mut edges, &b.nodes, be)
                    }
                    // a is not advanced, and not inserted
                    b_edge = b_edges.next();
                }
            },

        }
    }



}

fn store_gen(gen: &Generation) {
    info!("storing generation {}", gen.generation_sn);
    // TODO @incomplete: implement this
}

fn find_genome_by_id<'a>(genome_id: &GenomeId, groups: &'a Groups) -> Option<&'a Genome> {
    for group in groups.values() {
        for genome in group.genomes.values() {
            if &genome.genome_id == genome_id {
                return Some(genome);
            }
        }
    }
    return None;
}

fn calculate_fitness(nodes: &Nodes, edges: &Edges) -> f64 {
    1.0
    // let input_nodes = nodes.values().filter(|&node| node.kind == NodeKind::InputNode);
    // let output_nodes = nodes.values().filter(|&node| node.kind == NodeKind::InputNode);
    //
    // let mut results = HashMap::new();
    //
    // for output_node in output_nodes {
    //     walk_back(output_node, &mut results)
    // }
}

fn value_of(start_node: &Node, all_nodes: &Nodes, all_edges: &Edges, results: &mut HashMap<NodeId, f64>, inputs: &HashMap<NodeId, f64>) -> f64 {
    match results.get(&(start_node.node_id)) {
        Some(x) => *x,
        None => {
            let input_edges = all_edges.descending_values()
                .filter(|&edge| edge.out_node_id == start_node.node_id);

            let mut value = 0.0;
            for input_edge in input_edges {
                let in_node_value = value_of(&all_nodes[&(input_edge.in_node_id)], all_nodes, all_edges, results, inputs);
                value += in_node_value * input_edge.weight;
            }
            value += start_node.bias;

            results.insert(start_node.node_id, value);
            value
        }
    }
}
