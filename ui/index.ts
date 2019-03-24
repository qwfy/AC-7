import {Elm} from './elm_wrapper.js';

import embed, {vega} from 'vega-embed';

document.addEventListener("DOMContentLoaded", function () {
  const app = Elm.Main.init({
    node: document.getElementById('app'),
    // This argument is useless, it's there to satisfy elm's requirement.
    flags: ''
  });

  app.ports.flushGenomeGraphs.subscribe(function (graph) {
    document.getElementById('genome-graph-container').innerHTML = graph;
  });

  app.ports.speciesFitnessOverGeneration.subscribe(function (spec) {
    embed(
      '#species-fitness-over-generation',
      spec,
      {'mode': 'vega-lite'}
    ).then(function (result) {
      result.view.run();
    });
  });
});
