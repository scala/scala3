---
layout: doc-page
title: "Local Optimisations"
---

The Dotty compiler implements several local optimisations to generate faster bytecode. These optimisations can be enabled by compiling with the `-optimise` flag. The current best source of documentation about what tree transformations are performed under `-optimise` is the Scaladoc classes in the [localopt package](https://github.com/lampepfl/dotty/tree/master/compiler/src/dotty/tools/dotc/transform/localopt).

Local optimisations assumes no use of Java Reflection to mutate static final fields.
