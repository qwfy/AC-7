create schema public;

comment on schema public is 'standard public schema';

alter schema public owner to postgres;

create table run
(
	run_id text not null
		constraint run_pk
			primary key,
	time_started timestamp with time zone not null,
	time_stopped timestamp with time zone
);

comment on table run is 'Each row in this table represents a run';

alter table run owner to postgres;

create unique index run_run_id_uindex
	on run (run_id);

create table generation
(
	generation_id uuid not null,
	run_id text not null
		constraint generation_run_run_id_fk
			references run,
	sn integer not null
);

comment on table generation is 'Each row represents a single generation in the corresponding run';

alter table generation owner to postgres;

create unique index generation_generation_id_uindex
	on generation (generation_id);

create unique index generation_run_id_sn_uindex
	on generation (run_id, sn);

create table genome
(
	genome_id uuid not null
		constraint genome_pk
			primary key,
	generation_id uuid
		constraint genome_generation_generation_id_fk
			references generation (generation_id),
	original_fitness double precision not null,
	species_sn integer not null,
	graph text not null
);

alter table genome owner to postgres;

create table node
(
	node_id uuid not null
		constraint node_pk
			primary key,
	genome_id uuid
		constraint node_genome_genome_id_fk
			references genome
);

alter table node owner to postgres;

create table edge
(
	in_node_id uuid not null
		constraint edge_node_node_id_fk
			references node,
	out_node_id uuid not null
		constraint edge_node_node_id_fk_2
			references node,
	enabled boolean not null,
	weight double precision not null
);

alter table edge owner to postgres;

create view population(run_id, generation_sn, species_sn, genome_id, original_fitness, graph) as
SELECT run.run_id,
       generation.sn AS generation_sn,
       genome.species_sn,
       genome.genome_id,
       genome.original_fitness,
       genome.graph
FROM ((genome
    LEFT JOIN generation ON ((genome.generation_id = generation.generation_id)))
         LEFT JOIN run ON ((generation.run_id = run.run_id)));

alter table population owner to postgres;

create view run_info(run_id, time_started, time_stopped, species_sns, generation_sns) as
SELECT run.run_id,
       run.time_started,
       run.time_stopped,
       ssn.species_sns,
       gsn.generation_sns
FROM ((run
    LEFT JOIN (SELECT population.run_id,
                      array_agg(DISTINCT population.species_sn) AS species_sns
               FROM population
               GROUP BY population.run_id) ssn USING (run_id))
         LEFT JOIN (SELECT population.run_id,
                           array_agg(DISTINCT population.generation_sn) AS generation_sns
                    FROM population
                    GROUP BY population.run_id) gsn USING (run_id));

alter table run_info owner to postgres;

create function population_species_major(_run_id text, _generation_sns integer[], _species_sns integer[], _order_method text) returns TABLE(run_id_ text, species_sn_ integer, generations_ jsonb[])
	language plpgsql
as $fun$
declare
  order_clause text := null;
begin
  order_clause := case lower(_order_method)
                    when 'undefined_order' then ''
                    when 'asc' then $$ order by pop.genome -> 'original_fitness' asc $$
                    when 'desc' then $$ order by pop.genome -> 'original_fitness' desc $$
    end;
  if order_clause is null
  then
    raise exception 'Bad order method: `%`', _order_method;
  end if;

  return query execute format($$
    select groupped.run_id,
           groupped.species_sn,
           array_agg(groupped.genomes_with_generation_sn) as genomes_of_species
    from (select pop.run_id,
                 pop.species_sn,
                 jsonb_build_object(
                     'generation_sn', pop.species_sn,
                     'genomes', array_agg(pop.genome %s)) as genomes_with_generation_sn
          from (select run_id,
                       generation_sn,
                       species_sn,
                       jsonb_build_object(
                           'run_id', run_id,
                           'generation_sn', generation_sn,
                           'species_sn', species_sn,
                           'genome_id', genome_id,
                           'original_fitness', original_fitness,
                           'graph', graph) as genome
                from population
                where run_id = $1
                  and species_sn = any($2)
                  and generation_sn = any($3)
               ) as pop
          group by (pop.run_id, pop.generation_sn, pop.species_sn)
         ) as groupped
    group by (groupped.run_id, groupped.species_sn);
  $$, order_clause)
  using _run_id, _species_sns, _generation_sns;
end;
$fun$;

alter function population_species_major(text, integer[], integer[], text) owner to postgres;

create function population_generation_major(_run_id text, _generation_sns integer[], _species_sns integer[], _order_method text) returns TABLE(generation_sn_ integer, species_ jsonb[])
	language plpgsql
as $fun$
declare
  order_clause text := null;
begin
  order_clause := case lower(_order_method)
                    when 'undefined_order' then ''
                    when 'asc' then $$ order by pop.one_genome -> 'original_fitness' asc $$
                    when 'desc' then $$ order by pop.one_genome -> 'original_fitness' desc $$
    end;
  if order_clause is null
  then
    raise exception 'Bad order method: `%`', _order_method;
  end if;

  return query execute format($$
    select groupped.generation_sn,
           array_agg(groupped.one_species order by one_species->'species_sn' asc
                    ) as one_generation
    from (select pop.generation_sn,
                 jsonb_build_object(
                     'species_sn', pop.species_sn,
                     'genomes', array_agg(pop.one_genome %s)) as one_species
          from (select generation_sn,
                       species_sn,
                       jsonb_build_object(
                           'run_id', run_id,
                           'generation_sn', generation_sn,
                           'species_sn', species_sn,
                           'genome_id', genome_id,
                           'original_fitness', original_fitness,
                           'graph', graph) as one_genome
                from population
                where run_id = $1
                  and species_sn = any($2)
                  and generation_sn = any($3)
               ) as pop
          group by (pop.generation_sn, pop.species_sn)
         ) as groupped
    group by groupped.generation_sn
    order by groupped.generation_sn asc;
  $$, order_clause)
  using _run_id, _species_sns, _generation_sns;
end;
$fun$;

alter function population_generation_major(text, integer[], integer[], text) owner to postgres;
