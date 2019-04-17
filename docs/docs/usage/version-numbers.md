---
layout: doc-page
title: "Version numbers"
---

**This documentation is outdated! Please find the newer version [here](/docs/contributing/release.html)**.

Dotty uses multiple schemes for version numbering.

Stable releases have version numbers of the form `0.${x}.${y}`, where `x` is a main version and `y` is a bug-fix update id.

Release candidates version numbers have the form `0.${x}.${y}-RC${z}`. 
Every 6 weeks, the latest release candidate is promoted to stable and becomes version `0.${x}.${y}`.
The release candidates let library authors test their code in advance of each
release. Multiple release candidates may be released during each 6 weeks
period to fix regressions and are differentiated by `z`.

Nightlies have version numbers of the form `0.${x}.${y}-bin-${date}-${sha}-NIGHTLY`.
Every 6 weeks, the latest nightly is promoted to release candidate becomes version `0.${x}.${y}-RC1`.
