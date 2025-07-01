# Validation Framework

This is a validation framework for `s2s`. It uses validation cases that consisting of queries, input, and output shapes related by the `s2s` algorithm. For these test cases, the framework generates random input graphs conforming to the input shapes (RDF or Property Graphs encoded in RDF), executes the CONSTRUCT query (either SPARQL or G-CORE mapped-to SPARQL), and finally tests whether the result graph conforms to the given output shapes.

## Usage

Ensure Python is installed and the (virtual) environment initialized with all `requirements.txt`.

To initialize the validation data, first run the `s2s` test suite; this generates the validation cases in the `data` directory. These include shapes and queries, and some additional metadata such as vocabularies and executable [Shardik](https://github.com/pseifer/shardik) knowledge base files that can be used for debugging misbehaving samples.

To execute validation, see `./validation --help`. To perform a quick run, use `./validation --run-all`. This executes all test cases once, targeting 100 triples in each graph. The number of triples to generate can be set with `-n TRIPLES`; repetitions per sample can be configured with `-x SAMPLES`.
Fewer triples lead to better (runtime) performance. The tool tries to ensure that there are relevant configurations in the graph, but also reports on failure scenarios (empty output graphs, missing targets) that vacuously satisfy output shapes. Note, that increasing the number of triples *might* help in such cases, but there are also problem instances that always lead to these outcomes.

With `--render`, images of input and output graphs can be generated. This requires the `graphviz` tool installed and on the path. Note, that `--render` should only be used with `-x 1` (i.e., single runs) since only one rendering is kept on disc. Furthermore, it might not scale with arbitrary graph sizes.

The tool reports progress on stderr and results (CSV encoded) on stdout. A common invocation would therefore be, e.g., `./validate --run-all -x 100 > results.csv` to run 100 different instances of each sample.

Finally, `./validate --gather-results` summarizes any cached results from the `data` directory (i.e., essentially re-creates `results.csv` from the previous run — for each sample, CSV output is also stored there).

## Internals

Internally, graphs are generated from SHACL or ProGS shapes. The generation algorithm uses brute force: Generate graphs drawing from the vocabulary, find shape constraint violations, prune them, repeat. A second criterion is the presence of targets for all input shapes. Eventually, this produces random graphs that include targets for all input shapes, that also conform to all input shapes.

