# Add Error Code Docs

Canonical skill for creating or updating Scala 3 error-code docs in
`docs/_docs/reference/error-codes/`.

This file is the single source of truth. Keep other related files as pointers.

## Best-Effort Contract

- Prefer full mechanical validation with `project/scripts/checkErrorCodeSnippets.scala`.
- If the environment blocks full validation, still produce the best draft possible and
  clearly mark what was not verified.
- If a code is not reproducible from pure Scala snippets, follow the non-reproducible path
  below and keep `NonReproducibleErrorCodes` aligned in
  `project/scripts/checkErrorCodeSnippets.scala`.

## Required Output Shape

Use this template for each `E###.md` page:

````markdown
---
title: E###: <Short Title>
kind: Error
# since: 3.x.y   # required when code > E166
# until: 3.x.y   # required when the compiler ID is inactive
---
# E###: <Short Title>

<Explain when this diagnostic happens and why.>

## Example

```scala sc:fail sc-opts:-explain
<minimal snippet that triggers only E###>
```

### Error

```scala sc:nocompile
-- [E###] <Error|Warning>: example.scala:<line>:<col> -------------------------
<exact checker output here, including explanation text when present>
```

### Solution

```scala sc:compile
<direct fix of the example above>
```

<!-- SOURCE-ONLY: Remove the notice below once this page has been manually updated. -->
<aside class="warning">
    This reference page was created with LLM assistance - the description of the error code may not be accurate or cover all possible scenarios.
</aside>
````

For warning diagnostics, normally make the example fail under `-Werror`:

```scala sc:fail sc-opts:-Werror
<warning-producing snippet>
```

## Workflow

1. Establish target metadata.
- Find the compiler ID in `compiler/src/dotty/tools/dotc/reporting/ErrorMessageID.scala`.
- Use filename `docs/_docs/reference/error-codes/E###.md`.
- Set frontmatter `title` and `kind` (`Error` or `Warning`).
- Add `since` when `errorNumber > 166`.
- Add `until` when the ID is inactive.

2. Draft from a nearby entry.
- Keep section order: `## Example`, `### Error` or `### Warning`, `### Solution`.
- Keep snippets minimal and deterministic.
- Keep solutions as direct fixes of the failing scenario.

3. Apply snippet marker rules.
- Example: `sc:fail` (or `custom-sc:fail` only when required).
- Output block: `sc:nocompile`.
- Solution: `sc:compile` (or `custom-sc:compile` only when required).
- Include needed flags in snippet options (`sc-opts:-explain`, `sc-opts:-Werror`, etc.).

4. Keep output faithful.
- Store exact compiler output with `[E###]`.
- Include explain text when emitted.

5. Keep indexing consistent.
- Add new files to `docs/sidebar.yml` under `Error Codes` in numeric order.

## Validation Commands

From repository root:

```bash
DOC=docs/_docs/reference/error-codes/E###.md
```

One-time local compiler setup (rerun only when artifacts/version change):

```bash
sbtn scala3-bootstrapped/publishLocal
```

Cache local bootstrapped version:

```bash
VERSION_CACHE=/tmp/scala3-bootstrapped-version.txt
version=$(BOOTSTRAPPED_VERSION_CACHE="$VERSION_CACHE" project/scripts/get-bootstrapped-version.sh --refresh)
# later runs:
# version=$(BOOTSTRAPPED_VERSION_CACHE="$VERSION_CACHE" project/scripts/get-bootstrapped-version.sh)
```

Refresh `### Error` / `### Warning` output:

```bash
scala --server=false --scala-version "$version" \
  project/scripts/checkErrorCodeSnippets.scala -- "$DOC" --update-output
```

Run full validation:

```bash
scala --server=false --scala-version "$version" \
  project/scripts/checkErrorCodeSnippets.scala -- "$DOC" --verbose
```

Expected guarantees when validation runs:

- Example snippets fail and include the documented `E###`.
- Unexpected extra error codes are rejected.
- Output block matches compiler output.
- Solution snippets compile cleanly (`-Werror` enforced by checker).

## Completion Checklist

- Frontmatter and heading match `E###`.
- Examples produce only the documented code (or are explicitly non-reproducible).
- Output block includes exact checker output.
- Solutions resolve the same scenario and compile without warnings.
- Sidebar entry exists and is ordered.
