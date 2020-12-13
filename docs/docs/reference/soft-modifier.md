---
layout: doc-page
title: Soft Modifiers
---

A soft modifier is one of the identifiers `opaque`, `inline`, `open`, `transparent`, and `infix`.
<!-- 
TODO this is most likely outdated should at least contain `extension` in addition. 
Worth maintaining? or maybe better refer to internal/syntax.md ? 
-->

It is treated as a potential modifier of a definition, if it is followed by a hard modifier or a keyword combination starting a definition (`def`, `val`, `var`, `type`, `class`, `case class`, `trait`, `object`, `case object`, `enum`). Between the two words there may be a sequence of newline tokens and soft modifiers.

It is treated as a potential modifier of a parameter binding unless it is followed by `:`.

