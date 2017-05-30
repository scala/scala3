---
layout: doc-page
title: "Version numbers"
---

Dotty uses multiple schemes for version numbering.

Stable releases have version number of form `0.${x}.${y}`, where x is a main version and y is a bug-fix update id.

Snapshots have a version `0.${x}.${y}-RC${z}`. When snapshot is promoted to stable it will have version `0.${x}.${y}`.
Spanshots are updated by incrementing `${z}`.

Nightlies have a version `0.${x}.0-bin-SNAPSHOT-${sha}`.

