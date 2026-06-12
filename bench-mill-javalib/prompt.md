Spawn sub-agents to profile the compiler and optimize its hot-spots.

- Your job is **high-level orchestration**: run the steps below, review each agent's
  output, take over if one gets stuck.

- Pass each section's instructions to its agent **verbatim** — only add extra context,
  never strip information.

- Don't analyze profiles or dig into code yourself.

# 1. Initialization

Spawn one sub-agent to run `./mill bench-mill-javalib/profile.scala --new-iter`. This
starts a fresh `target/bench-mill-javalib/iter-N/`, records and analyzes 5 JFR
profiles, then removes the raw `.jfr` files and leaves `profile-summary.txt` plus
`profile-trees.txt` in `iter-N/run-0/`.

If the profile isn't clean, get a clean one before continuing. Then record the exact
baseline commit with `git rev-parse HEAD` and write it, together with the baseline run
path, into the iteration notes (for example `iter-N/baseline.txt` and later
`iter-N/research.md`). Review the new baseline plus `git log -p main..HEAD compiler/`
and earlier `iter-N/run-M/` artifacts such as `iter-N/research.md`, and propose **12
broad investigation areas** (algorithms, data structures, allocations, etc.).

# 2. Research

Spawn **12 parallel maximum-effort agents**, one per area, at most 6 running at a time. Require exhaustive
investigation before accepting their results: agents should spend time reading code,
walking call chains, checking profile-tree attribution, cross-referencing prior
research, and performing test runs with added instrumentation/counters/logging
before they commit to a proposal. Reject weak candidates during investigation.
If an agent returns an under-supported list, require more code and profile analysis
before accepting it.
Assign each research agent exactly one of `target/worktree/scala3-{1,2,3,4,5,6}` so they can
add temporary instrumentation and counters in their worktree for deeper runtime behavior
investigation. These research worktree edits are exploratory only and must not be
committed or cherry-picked.

Each must:

- Read `git log -p main..accepted compiler/` (past wins) and `main..rejected compiler/`
  (past failures — don't re-propose verbatim).

- Read prior research and profiles in earlier `iter-N/run-M/` folders.

- Correlate the codebase (algorithms, data structures, data flow) with JFR self% / tot%
  and `-Vprofile` / `-Ystats` data, including the profile-trees.txt caller-attribution
  forests when picking which row a candidate actually moves.

- Brainstorm a wide candidate pool, then **deeply investigate each before returning**:
  read the actual code at the call sites *and the callers/callees one level out*,
  cross-check the JFR self/tot% the proposal claims to move, confirm the shape isn't
  already shipped on `accepted`, confirm the same shape hasn't already been rejected on
  `rejected` or shown unviable in prior `iter-N/run-M/` research, and reason about whether the
  proposed mechanism actually fires often enough on this workload to be measurable.
  Validate any ideas by adding logging/instrumentation/counters to your worktree and 
  performing test runs to get actual data to justify or disprove them. 
  Drop candidates with broken invariants, wrong types, near-zero hit rates, or any
  mechanism-to-payoff claim that does not survive review.

- Write a `iter-N/research-$n-$topic.md` file containing **20 verified distinct high-level optimizations** — 
  substantial wins, not
  small cosmetic tweaks. If fewer than 20 survive verification, continue broadening
  into adjacent surfaces, re-reading the profile, and consulting prior research for
  unshipped ideas rather than padding with weak entries. Use this format for each
  proposal:

  ```
  Proposal: <title>
  Mechanism: <detailed explanation of the optimization, the affected code path, the
  workload condition that makes it hot, and why the proposed change should move the
  cited profile rows>

  Expected improvements:
  - <method/profile row>: <why this exact row should improve; cite profile tree, JFR,
    stats, or counter evidence>
  - <method/profile row>: <why this exact row should improve; cite profile tree, JFR,
    stats, or counter evidence>

  Expected regressions:
  - <method/profile row>: <why this exact row could regress or take shifted work; cite
    evidence, invariant, or risk>
  - None expected: <why no specific regression row is expected>

  Prior-history check: <accepted/rejected/prior-research result that makes this distinct>

  Verifier checks: <counters, profile rows, tests, or invariants the follow-up verifier
  should check>
  ```

After each research agent finishes its self-verification, spawn a **follow-up verification
sub-agent** for that area.

- The follow-up agent re-investigates the 20 proposals from a
  fresh perspective — independently re-reading the cited code, JFR rows, and prior
  accepted/rejected history — to stress-test claims, surface invariants the first pass
  missed, and dig deeper into the mechanism.

- Verification agents may use the same assigned worktree as the research agent they are
  checking, including any temporary instrumentation or counters needed to validate runtime
  behavior claims. 

- The verification agent should write a `iter-N/verification-$n-$topic.md` file containing a per-proposal verdict
  (legitimate / marginal / rejected) with reasoning.

- Do not defer borderline proposals because more research, counters, instrumentation,
  logging, or other data is needed. Each sub-agent has a worktree and should use it to
  run a profile with whatever counters, instrumentation, or logging is necessary to make a
  decision. Require each sub-agent to investigate its candidates thoroughly before it
  finishes.

Once each verification agent completes, validate the `research` and `verification` markdown files: 
it should have 10 proposals, and every proposal must include `Expected improvements`, `Expected
regressions`, `Prior-history check`, `Verifier checks`, and the follow-up verifier's
verdict and key finding. If any proposal is missing method-level row reasons or
rejected-proposal findings, rewrite the research file first. Once *every* pair of research and verification
markdown files are completed, begin spawning implementation agents to work on
the proposals.

# 3. Implementation

Spawn **three parallel implementation agents**, each picking one optimization. Keep three
in flight at all times: as soon as one finishes, immediately spawn the next on its freed
worktree until all proposed optimizations from step 2 have been analyzed.

- One optimization per commit.

- Optimization size can range from a micro-tweak to a cross-cutting change, as long as
  it pays for itself in the profile.

- Each agent profiles twice via `./mill bench-mill-javalib/profile.scala`
  (auto-writes to the next `iter-N/run-M/` slot in the current iter): first on the
  unchanged baseline commit, then on `baseline commit + one optimization`.

## Baseline discipline

- Record the iteration baseline commit and initial baseline profile run (`iter-N/run-0`).

- Before starting each optimization, reset the agent's assigned worktree to the baseline
  commit and re-profile that exact commit in the assigned worktree. This fresh
  per-agent baseline run is the only `--before` profile for that agent's optimization.

- Each implementation profile measures exactly `baseline commit + one optimization`
  against the same agent's fresh baseline profile, not against `iter-N/run-0`.

- Do not base implementation agents on the moving `accepted` branch during the same
  iteration.

- Do not profile the final `accepted` branch.

## Worktree workflow

- Assign each implementation agent exactly one of `target/worktree/scala3-{1,2,3,4}`.

- Agents edit, test, profile, and make exactly one local commit in their assigned
  worktree.

- Agents must not commit directly in the root `scala3` repo and must not move `accepted` or
  `rejected`.

- After profiling, each agent reports: local commit SHA, accept/reject verdict,
  per-agent baseline run directory, optimization profile run directory, delta output,
  and validation commands.

- Before cherry-picking any reported commit, the orchestrator should review and edit
  that commit message in the agent worktree. Compare the commit against recent
  `accepted` / `rejected` commit messages, remove implementation walkthroughs,
  process notes, profile paths, validation boilerplate, and irrelevant rows, and bring
  the message in line with the historical formatting conventions. The reviewer must
  compare the message under review to the commit message template below and to the rest
  of `git log bench-mill-javalib...HEAD` for consistency. Only cherry-pick the commit
  after this review agent says the message is ready.

- The orchestrator cherry-picks accepted local commits onto `accepted` and non-improvers
  onto `rejected` from the root `scala3`.

- If cherry-pick conflicts, the orchestrator resolves it or sends the task back to the
  agent on a refreshed worktree.

- Agents must not reset, rebase, or clean their assigned worktree after creating the
  local commit until the orchestrator confirms it was cherry-picked or abandoned.

- After confirmation, reset the worktree back to the iteration baseline commit before
  reusing it.

- Agents run `./mill bench-mill-javalib/profile.scala` concurrently. Do not use external
  profiling locks; `profile.scala` claims `run-M` directories atomically.

## Comparison + commit

Diff with `./mill bench-mill-javalib/delta.scala --before run-B --after run-M METHOD…`,
where `run-B` is the agent's fresh baseline profile and `run-M` is that same agent's
optimization profile. Replace both placeholders with the actual run labels. The delta
script reads both `profile-summary.txt` files and prints each method's isolated
`self% / tot% before → after  (Δ%, K.Kσ)`. Use **% time** and **std devs** — not
raw samples / wall-clock (too noisy). Cover affected methods, allocation share if
relevant, and any rows expected to regress. Do **not** run `bench.scala`.

**Go/no-go criterion:** a timing change counts as significant only when the larger of
the two profiles' std devs is smaller than the gap between their means — i.e.
`max(sd_before, sd_after) < |mean_after - mean_before|` (equivalently, σ > 1.0 in the
`delta.scala` output). If the gap is within the std devs, the change is noise; it
belongs on `rejected`.

If the change measurably improves performance, smoke-test the bootstrapped compiler +
stdlib before reporting an accepted local commit. If the change does not measurably
improve performance but is still useful to preserve as a rejected attempt, it must at
least pass compiler compilation before reporting a rejected local commit. Broken
attempts that do not compile should be reported as abandoned, not committed to
`rejected`.

Every local commit message must follow the branch style. Accepted subjects look like
`TreeTypeMap: inline hot nonbinding transforms`; rejected subjects start with
`REJECTED:`. Use this shape:

```
<Component>: <accepted summary>

<Mechanism and safety paragraph.>

Expected changes:
- <short method/profile row> self% or tot% should improve: <why this row should move>

- <short method/profile row> self% or tot% should regress: <why this row could move>

- No other regressions expected: <semantic guardrail or profile reason>

JFR profile deltas (5 repeats × 10 runs, mean ± stddev, iter-N/run-B → iter-N/run-M):
1. <short method/profile row> self%: <before> ± <sd> → <after> ± <sd>  (<delta>, <sigma>σ)
2. <short method/profile row>  tot%: <before> ± <sd> → <after> ± <sd>  (<delta>, <sigma>σ)
3. <short method/profile row> self%: <before> ± <sd> → below floor
4. <short method/profile row>  tot%: below floor → <after> ± <sd>

Estimated total speedup: <speedup> ± <uncertainty> (from <row-numbers> above)

Accepted. <Explain the decision from the measured rows without workflow details.>
```

Adapt the subject, summary, rows, speedup estimate, and verdict to the measured change,
but keep the same spacing and section order.

- The explanatory paragraph before `Expected changes:` must be 2-3 sentences and must
  explain what changed, why the code path is hot, why the mechanism improves that hot
  path, why it is safe, and any local background needed for review. Cite profile
  percentages, allocation amounts, hit rates, or counter values where possible.

- The `Expected changes:` bullets must name the specific short method rows whose self%
  or tot% time should improve or regress, and give the reason each row should move.

- Use short historical names such as `ClassDenotation.membersNamed` or
  `TypeComparer.recur`, not fully qualified `dotty.tools.dotc...` names, in both
  `Expected changes:` and `JFR profile deltas`.

- Use only direct affected rows, important caller confirmation rows, and meaningful
  regressions in the numbered JFR list.

- Each numbered JFR row must contain exactly one metric row: never combine self% and
  tot% in one numbered item with punctuation such as `; tot%`.

- In the JFR header, identify the compared profiles with only their `iter-N/run-M`
  labels. Do not include commit IDs, worktree names, or the full
  `target/bench-mill-javalib/` path.

- Measurement tables must use `×` and `→`, not ASCII `x` or `->`.

When estimating total speedup, sum the row deltas (negated; positive = improvement).
Use `self%` rows where possible because they partition runtime. If using `tot%`, only
sum siblings; never sum a caller and its callee. Compute uncertainty as
`σ = sqrt(Σ (sd_before_i² + sd_after_i²))`.

**Match the style of recent commits on the target branch exactly** — compare the
message under review with the template above and with the rest of
`git log bench-mill-javalib...HEAD` for consistency (subject line shape, body
structure, where the profile-delta section goes, how rejected commits are titled,
etc.). Don't restate irrelevant workflow details.

# Repeat

Continuously re-spawn implementation agents to replace any that finish, keeping three in flight
at all times, until all proposed optimizations from step 2 have completed.

# Misc

- All output under `target/bench-mill-javalib/` (anchored to the main worktree; linked
  worktrees write to the same shared dir).

- Place reusable scripts in `bench-mill-javalib/` and maintain ones already there.

- Don't touch git branches outside `main`, `accepted`, `rejected`, `bench-mill-javalib`.

- Make sure you monitor your subagents closely: ensure they are making progress, restart them if they get stuck or crash, but don't restart them if they are idle because they're waiting for a subprocess monitor or similar. Every 60s please check every subagent and its associated logs, output files, subprocesses, etc. to understand their progress. 
