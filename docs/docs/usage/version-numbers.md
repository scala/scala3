---
layout: doc-page
title: "Version numbers"
---

Dotty uses multiple schemes for version numbering.

Stable releases have version number of the form `0.${x}.${y}`, where x is a main version and y is a bug-fix update id.

Release candidates version numbers have the form `0.${x}.${y}-RC${z}`. Release candidates are updated by incrementing `${z}`.
When a release candidate is promoted to stable it becomes version `0.${x}.${y}`.

Nightlies have version numbers of the form `0.${x}.${y}-bin-${date}-${sha}-NIGHTLY`.

