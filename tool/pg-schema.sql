--
-- PostgreSQL database cluster dump
--

SET default_transaction_read_only = off;

SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;

--
-- Drop databases (except postgres and template1)
--





--
-- Drop roles
--

DROP ROLE IF EXISTS "authenticator";
DROP ROLE IF EXISTS "postgres";
DROP ROLE IF EXISTS "web_anon";


--
-- Roles
--

CREATE ROLE "authenticator";
ALTER ROLE "authenticator" WITH NOSUPERUSER NOINHERIT NOCREATEROLE NOCREATEDB LOGIN NOREPLICATION NOBYPASSRLS PASSWORD 'md5474bd882be35ad430e7267f1e778b68f';
CREATE ROLE "postgres";
ALTER ROLE "postgres" WITH SUPERUSER INHERIT CREATEROLE CREATEDB LOGIN REPLICATION BYPASSRLS PASSWORD 'md5c39183a5c79119d5a943c3b318f4eca9';
CREATE ROLE "web_anon";
ALTER ROLE "web_anon" WITH NOSUPERUSER INHERIT NOCREATEROLE NOCREATEDB NOLOGIN NOREPLICATION NOBYPASSRLS;


--
-- Role memberships
--

GRANT "web_anon" TO "authenticator" GRANTED BY "postgres";




--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1 (Debian 11.1-1.pgdg90+1)
-- Dumped by pg_dump version 11.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

UPDATE pg_catalog.pg_database SET datistemplate = false WHERE datname = 'template1';
DROP DATABASE "template1";
--
-- Name: template1; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE "template1" WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'en_US.utf8' LC_CTYPE = 'en_US.utf8';


ALTER DATABASE "template1" OWNER TO "postgres";

\connect "template1"

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE "template1"; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON DATABASE "template1" IS 'default template for new databases';


--
-- Name: template1; Type: DATABASE PROPERTIES; Schema: -; Owner: postgres
--

ALTER DATABASE "template1" IS_TEMPLATE = true;


\connect "template1"

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE "template1"; Type: ACL; Schema: -; Owner: postgres
--

REVOKE CONNECT,TEMPORARY ON DATABASE "template1" FROM PUBLIC;
GRANT CONNECT ON DATABASE "template1" TO PUBLIC;


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1 (Debian 11.1-1.pgdg90+1)
-- Dumped by pg_dump version 11.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

DROP DATABASE "postgres";
--
-- Name: postgres; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE "postgres" WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'en_US.utf8' LC_CTYPE = 'en_US.utf8';


ALTER DATABASE "postgres" OWNER TO "postgres";

\connect "postgres"

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE "postgres"; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON DATABASE "postgres" IS 'default administrative connection database';


--
-- Name: population_generation_major("text", integer[], integer[], "text"); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION "public"."population_generation_major"("_run_id" "text", "_generation_sns" integer[], "_species_sns" integer[], "_order_method" "text") RETURNS TABLE("run_id_" "text", "generation_sn_" integer, "species_" "jsonb"[])
    LANGUAGE "plpgsql"
    AS $_$
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
           groupped.generation_sn,
           array_agg(groupped.genomes_with_species_sn) as genomes_of_generation
    from (select pop.run_id,
                 pop.generation_sn,
                 jsonb_build_object(
                     'species_sn', pop.species_sn,
                     'genomes', array_agg(pop.genome %s)) as genomes_with_species_sn
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
    group by (groupped.run_id, groupped.generation_sn);
  $$, order_clause)
  using _run_id, _species_sns, _generation_sns;
end;
$_$;


ALTER FUNCTION "public"."population_generation_major"("_run_id" "text", "_generation_sns" integer[], "_species_sns" integer[], "_order_method" "text") OWNER TO "postgres";

--
-- Name: population_species_major("text", integer[], integer[], "text"); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION "public"."population_species_major"("_run_id" "text", "_generation_sns" integer[], "_species_sns" integer[], "_order_method" "text") RETURNS TABLE("run_id_" "text", "species_sn_" integer, "generations_" "jsonb"[])
    LANGUAGE "plpgsql"
    AS $_$
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
$_$;


ALTER FUNCTION "public"."population_species_major"("_run_id" "text", "_generation_sns" integer[], "_species_sns" integer[], "_order_method" "text") OWNER TO "postgres";

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: edge; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."edge" (
    "in_node_id" "uuid" NOT NULL,
    "out_node_id" "uuid" NOT NULL,
    "enabled" boolean NOT NULL,
    "weight" double precision NOT NULL
);


ALTER TABLE "public"."edge" OWNER TO "postgres";

--
-- Name: generation; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."generation" (
    "generation_id" "uuid" NOT NULL,
    "run_id" "text" NOT NULL,
    "sn" integer NOT NULL
);


ALTER TABLE "public"."generation" OWNER TO "postgres";

--
-- Name: TABLE "generation"; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE "public"."generation" IS 'Each row represents a single generation in the corresponding run';


--
-- Name: genome; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."genome" (
    "genome_id" "uuid" NOT NULL,
    "generation_id" "uuid",
    "original_fitness" double precision NOT NULL,
    "species_sn" integer NOT NULL,
    "graph" "text" NOT NULL
);


ALTER TABLE "public"."genome" OWNER TO "postgres";

--
-- Name: node; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."node" (
    "node_id" "uuid" NOT NULL,
    "genome_id" "uuid"
);


ALTER TABLE "public"."node" OWNER TO "postgres";

--
-- Name: run; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."run" (
    "run_id" "text" NOT NULL,
    "time_started" timestamp with time zone NOT NULL,
    "time_stopped" timestamp with time zone
);


ALTER TABLE "public"."run" OWNER TO "postgres";

--
-- Name: TABLE "run"; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE "public"."run" IS 'Each row in this table represents a run';


--
-- Name: population; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW "public"."population" AS
 SELECT "run"."run_id",
    "generation"."sn" AS "generation_sn",
    "genome"."species_sn",
    "genome"."genome_id",
    "genome"."original_fitness",
    "genome"."graph"
   FROM (("public"."genome"
     LEFT JOIN "public"."generation" ON (("genome"."generation_id" = "generation"."generation_id")))
     LEFT JOIN "public"."run" ON (("generation"."run_id" = "run"."run_id")));


ALTER TABLE "public"."population" OWNER TO "postgres";

--
-- Name: run_info; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW "public"."run_info" AS
 SELECT "run"."run_id",
    "run"."time_started",
    "run"."time_stopped",
    "ssn"."species_sns",
    "gsn"."generation_sns"
   FROM (("public"."run"
     LEFT JOIN ( SELECT "population"."run_id",
            "array_agg"(DISTINCT "population"."species_sn") AS "species_sns"
           FROM "public"."population"
          GROUP BY "population"."run_id") "ssn" USING ("run_id"))
     LEFT JOIN ( SELECT "population"."run_id",
            "array_agg"(DISTINCT "population"."generation_sn") AS "generation_sns"
           FROM "public"."population"
          GROUP BY "population"."run_id") "gsn" USING ("run_id"));


ALTER TABLE "public"."run_info" OWNER TO "postgres";

--
-- Name: genome genome_pk; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."genome"
    ADD CONSTRAINT "genome_pk" PRIMARY KEY ("genome_id");


--
-- Name: node node_pk; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."node"
    ADD CONSTRAINT "node_pk" PRIMARY KEY ("node_id");


--
-- Name: run run_pk; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."run"
    ADD CONSTRAINT "run_pk" PRIMARY KEY ("run_id");


--
-- Name: generation_generation_id_uindex; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX "generation_generation_id_uindex" ON "public"."generation" USING "btree" ("generation_id");


--
-- Name: generation_run_id_sn_uindex; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX "generation_run_id_sn_uindex" ON "public"."generation" USING "btree" ("run_id", "sn");


--
-- Name: run_run_id_uindex; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX "run_run_id_uindex" ON "public"."run" USING "btree" ("run_id");


--
-- Name: edge edge_node_node_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."edge"
    ADD CONSTRAINT "edge_node_node_id_fk" FOREIGN KEY ("in_node_id") REFERENCES "public"."node"("node_id");


--
-- Name: edge edge_node_node_id_fk_2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."edge"
    ADD CONSTRAINT "edge_node_node_id_fk_2" FOREIGN KEY ("out_node_id") REFERENCES "public"."node"("node_id");


--
-- Name: generation generation_run_run_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."generation"
    ADD CONSTRAINT "generation_run_run_id_fk" FOREIGN KEY ("run_id") REFERENCES "public"."run"("run_id");


--
-- Name: genome genome_generation_generation_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."genome"
    ADD CONSTRAINT "genome_generation_generation_id_fk" FOREIGN KEY ("generation_id") REFERENCES "public"."generation"("generation_id");


--
-- Name: node node_genome_genome_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "public"."node"
    ADD CONSTRAINT "node_genome_genome_id_fk" FOREIGN KEY ("genome_id") REFERENCES "public"."genome"("genome_id");


--
-- Name: SCHEMA "public"; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA "public" TO "web_anon";


--
-- Name: TABLE "edge"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."edge" TO "web_anon";


--
-- Name: TABLE "generation"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."generation" TO "web_anon";


--
-- Name: TABLE "genome"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."genome" TO "web_anon";


--
-- Name: TABLE "node"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."node" TO "web_anon";


--
-- Name: TABLE "run"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."run" TO "web_anon";


--
-- Name: TABLE "population"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."population" TO "web_anon";


--
-- Name: TABLE "run_info"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."run_info" TO "web_anon";


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database cluster dump complete
--

