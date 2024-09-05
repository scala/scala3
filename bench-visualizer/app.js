const benchmarks = {
  dotty: {
    title: "Dotty",
  },
  dottySbt: {
    title: "Dotty SBT",
  },
  stdlib: {
    title: "Stdlib",
  },
  scalap: {
    title: "scalap",
  },
  re2s: {
    title: "re2",
  },
  implicitCacheBootstrapped: {
    title: "implicit cache I - bootstrapped",
  },
  implicitCacheTasty: {
    title: "implicit cache I - from tasty",
  },
};

const main = selectOrThrow("main");

document.addEventListener("DOMContentLoaded", init);
function init() {
  // We use event delegation to handle changes, so we only need to attach these
  // event listeners once and for all.
  main.addEventListener("change", handleAggregatedOptionsChange);

  route();
}

window.addEventListener("hashchange", route);
function route() {
  const hash = window.location.hash;
  if (hash === "") {
    renderAggregated();
  } else if (hash.startsWith("#compare/")) {
    const commits = hash.slice("#compare/".length).split(",");
    renderComparison(commits);
  }
}

/**
 * Selects an element. Throws an error if no element is found.
 *
 * @param {string} selector CSS selector.
 * @returns {Element} The selected element.
 */
function selectOrThrow(selector) {
  const element = document.querySelector(selector);
  if (element === null) throw new Error(`No element found for selector "${selector}".`);
  return element;
}

/*******************/
/* Aggregated view */
/*******************/

/**
 * Renders the aggregated view. This view displays a graph for each benchmark,
 * showing the median value of the benchmark over time. Calling this function
 * renders the aggregated view in the <main> element.
 *
 * @returns {void}
 */
function renderAggregated() {
  const options = loadAggregatedOptions();
  const checked = (value) => (value ? "checked" : "");
  main.innerHTML = /*html*/`
    <h1>Dotty Benchmarks</h1>
    <form id="aggregated-options">
      <div class="checkbox-group">
        <label>
          <p><input type="checkbox" id="spaceEvenly" ${checked(
            options.spaceEvenly
          )}> Evenly space values horizontally</p>
          <p class="description">When checked, values are displayed at constant intervals on the x-axis. Dates are shown for information, but the x-axis is not linear in time and its range might differ between benchmarks. When unchecked, the x-axis represents commit dates, with values spaced accordingly.</p>
        </label>
      </div>
      <div class="checkbox-group">
        <label>
          <p><input type="checkbox" id="startYAxisAtZero" ${checked(options.startYAxisAtZero)}> Start y-axis at 0</p>
          <p class="description">When checked, the y-axis starts at 0. When unchecked, the y-axis range is automatically determined based on the data.</p>
        </label>
      </div>
      <div class="checkbox-group">
        <label>
          <p><input type="checkbox" id="headOnly" ${checked(options.headOnly)}> Only display recent values</p>
          <p class="description">When checked, only recent values are displayed. If values are spaced evenly, the last 100 values are shown. If not, the last three months are displayed. When unchecked, all historical values are shown. Note that loading historical data may take a while.</p>
        </label>
      </div>
      <div class="checkbox-group">
        <label>
          <p><input type="checkbox" id="errorBars" ${checked(options.errorBars)}> Show error bars</p>
          <p class="description">When checked, error bars are displayed for each data point, with whiskers representing the minimum and maximum values.</p>
        </label>
      </div>
      <div class="checkbox-group">
        <label>
          <p><input type="checkbox" id="movingAverage" ${checked(options.movingAverage)}> Show moving average</p>
          <p class="description">When checked, the moving average of the median values is displayed. The window size is 10 points.</p>
        </label>
      </div>
    </form>
    <div class="benchs">
    ${Object.entries(benchmarks)
      .map(
        ([id, bench]) => `
          <div class="bench">
            <h2>${bench.title}</h2>
            <div id="${id}"></div>
          </div>
        `
      )
      .join("")}
    </div>
  `;

  renderAggregatedGraphs(options);
}

/**
 * Options for the aggregated view. See the HTML documentation in
 * `renderAggregated` for a description of each option.
 *
 * @typedef {{
 *   spaceEvenly: boolean,
 *   startYAxisAtZero: boolean,
 *   headOnly: boolean,
 *   errorBars: boolean,
 *   movingAverage: boolean
 * }} AggregatedOptions
 */

/** Saves the aggregated view options to local storage.
 *
 * @param {AggregatedOptions} options
 * @returns {void}
 */
function saveAggregatedOptions(options) {
  window.localStorage.setItem("options", JSON.stringify(options));
}

/** Loads the aggregated view options from local storage.
 *
 * @returns {AggregatedOptions}
 */
function loadAggregatedOptions() {
  const options = window.localStorage.getItem("options");
  if (options === null) {
    return {
      spaceEvenly: true,
      startYAxisAtZero: false,
      headOnly: true,
      errorBars: true,
      movingAverage: true,
    };
  } else {
    return JSON.parse(options);
  }
}

/**
 * Renders the aggregated graphs. Each graph is rendered in a <div> with the
 * corresponding benchmark ID: for example, the graph for the "dotty" benchmark
 * is rendered in the <div id="dotty"> element. These <div>s must exist in the
 * DOM (or have been scheduled to be created) before calling this function.
 *
 * @param {AggregatedOptions} options
 * @returns {void}
 */
function renderAggregatedGraphs(options) {
  for (const id of Object.keys(benchmarks)) {
    renderAggregatedGraph(id, options);
  }
}

/**
 * Renders an aggregated graph for the benchmark with the given ID. The graph
 * is rendered in the <div> element with the corresponding ID. If a graph is
 * already efficiently it is updated with the new data.
 *
 * @param {string} id Benchmark ID.
 * @param {AggregatedOptions} options
 * @returns {Promise<void>}
 */
async function renderAggregatedGraph(id, options) {
  const threeMonthsAgo = new Date();
  threeMonthsAgo.setMonth(threeMonthsAgo.getMonth() - 3);

  const filter = options.headOnly ? "last100" : "all";
  let data = await loadAggregatedData(id, filter);
  if (options.headOnly && !options.spaceEvenly) {
    // Show the last three months of data. Note that we only loaded the last 100
    // values, but this should generally be enough to cover three months.
    const start = data.times.findIndex((time) => new Date(time) > threeMonthsAgo);
    data = sliceAggregatedData(data, start);
  }
  const { indices, times, commits, prs, medians, mins, maxs, movingAverage } = data;

  const scatterTrace = {
    // Spacing evenly means using indices as x values (but we override the tick
    // labels to show dates if the option is enabled). Otherwise, we use the
    // times as x values. Note that in this case, Plotly will automatically
    // infer that we display a time series and format the x-axis accordingly.
    x: options.spaceEvenly ? indices : times,
    y: medians,
    hoverinfo: "all",
    hovertemplate: "<b>Iteration time:</b> %{y} ms<br><b>Commit:</b> %{customdata}<br><b>PR:</b> %{meta}",
    meta: prs,
    customdata: commits,
    mode: "markers", // don't connect points with lines
    type: "scatter",
    name: "Median",
  };
  if (options.errorBars) {
    scatterTrace.error_y = {
      type: "data",
      array: maxs, // difference between median and max
      arrayminus: mins, // difference between median and min
      visible: true,
    };
  }

  const movingAverageTrace = {
    x: options.spaceEvenly ? indices : times,
    y: movingAverage,
    mode: "lines",
    //line: { shape: "spline" }, // uncomment to show a spline
    type: "scatter",
    name: "Moving average",
  };

  const layout = {
    xaxis: {
      zeroline: false,
      title: "Date",
    },
    yaxis: {
      zeroline: true,
      title: "Time (ms)",
    },
    legend: {
      orientation: "h",
      yanchor: "bottom",
      y: 1.02,
      xanchor: "right",
      x: 1,
    },
  };
  if (options.spaceEvenly) {
    // Show 6 labels on the x-axis.
    const labelInterval = Math.floor((indices.length - 1) / 5);
    const filteredIndices = indices.filter((_, i) => i % labelInterval === 0);
    const filteredTimes = times
      .filter((_, i) => i % labelInterval === 0)
      .map((time) =>
        // Show only the date part of the ISO string, for example, "2024-08-14".
        new Date(time).toISOString().slice(0, 10)
      );
    layout.xaxis.tickvals = filteredIndices;
    layout.xaxis.ticktext = filteredTimes;

    // It is debatable whether to show tick labels when values are spaced
    // evenly: the x-axis is not linear in time, so showing dates might be
    // misleading. On the other hand, they provide useful context.
    layout.xaxis.showticklabels = true;
  } else {
    if (options.headOnly) {
      layout.xaxis.range = [threeMonthsAgo.toISOString(), new Date().toISOString()];
    } else {
      // Make sure that all graphs have the same x-axis range.
      layout.xaxis.range = ["2021-01-01", new Date().toISOString()];
    }
  }
  if (options.startYAxisAtZero) {
    const maxMedian = Math.max(...medians);
    layout.yaxis.range = [0, maxMedian * 1.1];
  }

  /** @type {any[]} */ const traces = [scatterTrace];
  if (options.movingAverage) {
    traces.push(movingAverageTrace);
  }

  // Using `PlotlyPlotly.react` instead of `Plotly.newPlot` allows to update the
  // graph without recreating it from scratch if a graph already exists.
  // Expects `Plotly` to be available in the global scope.
  // @ts-ignore
  const graphElement = await Plotly.react(id, traces, layout);

  graphElement.removeAllListeners("plotly_click");
  graphElement.on("plotly_click", (data) => {
    const point = data.points[0];
    if (point !== undefined && point.meta !== undefined) {
      window.open(`https://github.com/scala/scala3/pull/${point.meta}`);
    }
  });
}

/**
 * Number of points to use for the moving average. Used by `loadAggregatedData`.
 */
const movingAverageWindow = 10;

/**
 * Parsed aggregated data for a benchmark. Each field corresponds to a column. Each array has the same length.
 *
 * @typedef {{
 *   indices: number[],
 *   times: string[],
 *   commits: string[],
 *   prs: string[],
 *   mins: number[],
 *   medians: number[],
 *   maxs: number[],
 *   movingAverage: number[]
 * }} AggregatedData
 */

/**
 * Loads aggregated data for the benchmark with the given ID. This function is
 * memoized.
 *
 * @param {string} id Benchmark ID.
 * @param {"all"|"last100"} filter Filter to apply.
 * @returns {Promise<AggregatedData>} Aggregated data columns.
 */
const loadAggregatedData = memo(
  async function (id, filter) {
    console.log("Loading aggregated data for", id, filter);
    const path = `data/aggregated/${filter}/${id}.csv`;
    const response = await fetch(path);
    const data = await response.text();
    const times = [];
    const commits = [];
    const prs = [];
    const mins = [];
    const medians = [];
    const maxs = [];

    for (const line of data.split("\n")) {
      const [time, commit, pr, min, median, max] = line.split(",");
      if (line === "") continue;
      times.push(time);
      commits.push(commit);
      prs.push(pr);
      mins.push(Number(median) - Number(min));
      maxs.push(Number(max) - Number(median));
      medians.push(Number(median));
    }

    const indices = times.map((_, i) => i);
    const movingAverage = indices.map((i) => {
      let sum = 0;
      let count = 0;
      for (let j = Math.max(0, i - movingAverageWindow + 1); j <= i; j++) {
        sum += medians[j];
        count++;
      }
      return sum / count;
    });

    const res = { indices, times, commits, prs, mins, medians, maxs, movingAverage };
    return res;
  },
  ([id, filter]) => `${id}-${filter}`
);

/**
 * Slices aggregated data, keeping only points starting from the `n`-th one to
 * the end.
 *
 * @param {AggregatedData} data Aggregated data.
 * @param {number} n Index of the first point to keep.
 * @returns {AggregatedData} Sliced aggregated data.
 */
function sliceAggregatedData(data, n) {
  return {
    indices: data.indices.slice(n),
    times: data.times.slice(n),
    commits: data.commits.slice(n),
    prs: data.prs.slice(n),
    mins: data.mins.slice(n),
    medians: data.medians.slice(n),
    maxs: data.maxs.slice(n),
    movingAverage: data.movingAverage.slice(n),
  };
}

/**
 * Handles changes to the aggregated view options. This function is bound
 * to the "change" event on the <main> element.
 *
 * @param {Event} e Change event.
 * @returns {void}
 */
function handleAggregatedOptionsChange(e) {
  // Check if the target is a checkbox that is a child of <form id="aggregated-options">.
  if (
    e.target instanceof HTMLInputElement &&
    e.target.type === "checkbox" &&
    e.target.closest("#aggregated-options") !== null
  ) {
    // IDs of the checkboxes are the same as the options object keys.
    const newOptions = { ...loadAggregatedOptions(), [e.target.id]: e.target.checked };
    saveAggregatedOptions(newOptions);
    renderAggregatedGraphs(newOptions);
  }
}

/*******************/
/* Comparison view */
/*******************/

/**
 * Renders the comparison view. This view shows the distribution of runtimes as
 * box plots for a given list of commits. Runtimes are normalized to the first
 * commit to allow for an easier comparison across benchmarks. Calling this
 * function renders the comparison view in the <main> element.
 *
 * @param {string[]} commits
 * @returns {void}
 */
function renderComparison(commits) {
  main.innerHTML = /*html*/ `
    <h1>Dotty Benchmarks</h1>
    <h2>Compare ${commits.join(", ")}</h2>
    <div id="compare"></div>
  `;

  renderComparisonGraph(commits);
}

/**
 * Map from benchmark ID to array of measurements.
 *
 * @typedef {Map<string, number[]>} DetailedData
 */

/**
 * Renders a comparison graph for the given commits. The graph is rendered in
 * the <div id="compare"> element.
 *
 * @param {string[]} commits
 * @returns {Promise<void>}
 */
async function renderComparisonGraph(commits) {
  const data = await Promise.all(commits.map(loadDetailedData));

  const firstCommitMedians = new Map();
  for (const benchmark of Object.keys(benchmarks)) {
    const firstCommitValues = data[0].get(benchmark);
    if (firstCommitValues === undefined) continue;
    firstCommitMedians.set(benchmark, median(firstCommitValues));
  }

  // Prepare traces for Plotly
  const traces = [];

  for (let i = 0; i < commits.length; i++) {
    /** @type {string[]} */ let x = [];
    /** @type {number[]} */ let y = [];

    for (const benchmark of Object.keys(benchmarks)) {
      const firstCommitMedian = firstCommitMedians.get(benchmark);
      const values = data[i].get(benchmark);
      if (values === undefined) continue;
      const scaledValues = values.map((value) => value / firstCommitMedian);
      x = x.concat(Array(scaledValues.length).fill(benchmark));
      y = y.concat(scaledValues);
    }

    traces.push({
      x,
      y,
      type: "box",
      name: commits[i],
      //boxpoints: "all", // TODO(mbovel): add an option for this
      jitter: 0.8,
      pointpos: 0,
      boxvisible: false,
    });
  }

  // Layout with emphasized 1-line
  const layout = {
    title: "Benchmark Comparison",
    yaxis: {
      title: "Relative to First Commit Median",
      zeroline: false,
      showline: true,
      showgrid: true,
    },
    legend: {
      orientation: "h",
      yanchor: "bottom",
      y: 1.02,
      xanchor: "right",
      x: 1,
    },
    shapes: [
      {
        type: "line",
        x0: 0,
        x1: 1,
        y0: 1,
        y1: 1,
        xref: "paper",
        yref: "y",
        line: { color: "black", width: 1 },
      },
    ],
    boxmode: "group",
  };

  // Render the plot
  // @ts-ignore
  Plotly.react("compare", traces, layout);
}

/**
 * Loads detailed data for the given commit. This function is memoized.
 *
 * @param {string} commit Commit hash.
 * @returns {Promise<DetailedData>}
 */
const loadDetailedData = memo(
  async function (commit) {
    console.log("Loading detailed data for", commit);
    const response = await fetch(`data/detailed/${commit}.csv`);
    const data = await response.text();
    const res = new Map();
    for (const line of data.split("\n")) {
      const [benchmark, , , values] = line.split(",");
      if (line === "") continue;
      res.set(benchmark, values.split(" ").map(Number));
    }
    return res;
  },
  ([commit]) => commit
);

/**
 * Calculates the median of an array of numbers.
 *
 * @param {number[]} numbers Array of numbers.
 * @returns {number} Median of the numbers.
 */
function median(numbers) {
  numbers.sort((a, b) => a - b);
  const mid = Math.floor(numbers.length / 2);
  return numbers.length % 2 !== 0 ? numbers[mid] : (numbers[mid - 1] + numbers[mid]) / 2;
}

/**
 * Memoize the given function.
 * @param {*} fn Function to memoize
 * @param {*} keyFn Function to generate the cache key from the arguments
 * @returns {function} Memoized function
 */
function memo(fn, keyFn) {
  const cache = new Map();
  return function (...args) {
    const key = keyFn(args);
    if (cache.has(key)) {
      return cache.get(key);
    } else {
      const result = fn(...args);
      cache.set(key, result);
      return result;
    }
  };
}
