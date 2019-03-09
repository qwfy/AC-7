create table run
(
  run_id       text                     not null
    constraint run_pk
      primary key,
  time_started timestamp with time zone not null,
  time_stopped timestamp with time zone
);

comment on table run is 'Each row in this table represents a run';

alter table run
  owner to postgres;

create unique index run_run_id_uindex
  on run (run_id);

create table generation
(
  generation_id uuid    not null,
  run_id        text    not null
    constraint generation_run_run_id_fk
      references run,
  sn            integer not null
);

comment on table generation is 'Each row represents a single generation in the corresponding run';

alter table generation
  owner to postgres;

create unique index generation_generation_id_uindex
  on generation (generation_id);

create unique index generation_run_id_sn_uindex
  on generation (run_id, sn);

create table genome
(
  genome_id        uuid             not null
    constraint genome_pk
      primary key,
  generation_id    uuid
    constraint genome_generation_generation_id_fk
      references generation (generation_id),
  original_fitness double precision not null,
  species_sn       integer          not null,
  graph            text             not null
);

alter table genome
  owner to postgres;

create table node
(
  node_id   uuid not null
    constraint node_pk
      primary key,
  genome_id uuid
    constraint node_genome_genome_id_fk
      references genome
);

alter table node
  owner to postgres;

create table edge
(
  in_node_id  uuid             not null
    constraint edge_node_node_id_fk
      references node,
  out_node_id uuid             not null
    constraint edge_node_node_id_fk_2
      references node,
  enabled     boolean          not null,
  weight      double precision not null
);

alter table edge
  owner to postgres;


