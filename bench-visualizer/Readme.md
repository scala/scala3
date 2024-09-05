# Dotty Benchmarks Vizualizer

This vizualizer is a single-page vanilla JavaScript application that uses [Plotly](https://plotly.com/javascript/) to display the results of the Dotty benchmarks.

It provides two views:

- <http://localhost:8080/>: The _aggregated view_ shows the runtime over time for each benchmark as scatter plots.
- <http://localhost:8080/#compare/1790bb5750,976133a33a> (for example): The _compare view_ shows the distribution of runtimes as box plots for a given list of commits. In this view, runtimes are normalized to the first commit to allow for an easier comparison across benchmarks.

## Dependencies

- [`scala-cli`](https://scala-cli.virtuslab.org) is required to build the data files used by the vizualizer.

## Build

Run the following command from the project root:

```bash
scala-cli vizualizer/build.scala
```

## Run

You need to spawn a local server to serve the vizualizer.

For example, to do so with Python, run the following command:

```bash
cd vizualizer
python3 -m http.server 8080
```

Then open the browser at `http://localhost:8080`.
