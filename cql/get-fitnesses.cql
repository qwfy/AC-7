// Get fitnesses
match
  (run:Run) <-[:GENERATION_OF]-
  (gen:Generation) <-[:SPECIES_OF]-
  (species:Species) <-[:GENOME_OF]-
  (genome:Genome)
  where run.runId = ""
  return [genome.genomeId, genome.originalFitness]
  order by genome.originalFitness desc
