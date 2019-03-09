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

ALTER TABLE IF EXISTS ONLY "public"."node" DROP CONSTRAINT IF EXISTS "node_genome_genome_id_fk";
ALTER TABLE IF EXISTS ONLY "public"."genome" DROP CONSTRAINT IF EXISTS "genome_generation_generation_id_fk";
ALTER TABLE IF EXISTS ONLY "public"."generation" DROP CONSTRAINT IF EXISTS "generation_run_run_id_fk";
ALTER TABLE IF EXISTS ONLY "public"."edge" DROP CONSTRAINT IF EXISTS "edge_node_node_id_fk_2";
ALTER TABLE IF EXISTS ONLY "public"."edge" DROP CONSTRAINT IF EXISTS "edge_node_node_id_fk";
DROP INDEX IF EXISTS "public"."run_run_id_uindex";
DROP INDEX IF EXISTS "public"."generation_run_id_sn_uindex";
DROP INDEX IF EXISTS "public"."generation_generation_id_uindex";
ALTER TABLE IF EXISTS ONLY "public"."run" DROP CONSTRAINT IF EXISTS "run_pk";
ALTER TABLE IF EXISTS ONLY "public"."node" DROP CONSTRAINT IF EXISTS "node_pk";
ALTER TABLE IF EXISTS ONLY "public"."genome" DROP CONSTRAINT IF EXISTS "genome_pk";
DROP VIEW IF EXISTS "public"."species_of_generation";
DROP VIEW IF EXISTS "public"."run_info";
DROP TABLE IF EXISTS "public"."node";
DROP VIEW IF EXISTS "public"."generation_of_species";
DROP VIEW IF EXISTS "public"."population";
DROP TABLE IF EXISTS "public"."run";
DROP TABLE IF EXISTS "public"."genome";
DROP TABLE IF EXISTS "public"."generation";
DROP TABLE IF EXISTS "public"."edge";
DROP SCHEMA IF EXISTS "public";
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA "public";


ALTER SCHEMA "public" OWNER TO "postgres";

--
-- Name: SCHEMA "public"; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA "public" IS 'standard public schema';


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
 SELECT "genome"."genome_id",
    "genome"."species_sn",
    "generation"."sn" AS "generation_sn",
    "run"."run_id"
   FROM (("public"."genome"
     LEFT JOIN "public"."generation" ON (("genome"."generation_id" = "generation"."generation_id")))
     LEFT JOIN "public"."run" ON (("generation"."run_id" = "run"."run_id")));


ALTER TABLE "public"."population" OWNER TO "postgres";

--
-- Name: generation_of_species; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW "public"."generation_of_species" AS
 SELECT "population"."run_id",
    "population"."species_sn",
    "array_agg"(DISTINCT "population"."generation_sn") AS "all_generations"
   FROM "public"."population"
  GROUP BY "population"."run_id", "population"."species_sn";


ALTER TABLE "public"."generation_of_species" OWNER TO "postgres";

--
-- Name: node; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "public"."node" (
    "node_id" "uuid" NOT NULL,
    "genome_id" "uuid"
);


ALTER TABLE "public"."node" OWNER TO "postgres";

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
-- Name: species_of_generation; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW "public"."species_of_generation" AS
 SELECT "population"."run_id",
    "population"."generation_sn",
    "array_agg"(DISTINCT "population"."species_sn") AS "all_species"
   FROM "public"."population"
  GROUP BY "population"."run_id", "population"."generation_sn";


ALTER TABLE "public"."species_of_generation" OWNER TO "postgres";

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
-- Name: TABLE "run"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."run" TO "web_anon";


--
-- Name: TABLE "population"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."population" TO "web_anon";


--
-- Name: TABLE "generation_of_species"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."generation_of_species" TO "web_anon";


--
-- Name: TABLE "node"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."node" TO "web_anon";


--
-- Name: TABLE "run_info"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."run_info" TO "web_anon";


--
-- Name: TABLE "species_of_generation"; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE "public"."species_of_generation" TO "web_anon";


--
-- PostgreSQL database dump complete
--

