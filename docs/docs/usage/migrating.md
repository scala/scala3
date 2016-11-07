---
layout: default
title: "Migrating to Dotty"
---

### Minor tweaks ###
 * `sym.linkedClassOfClass` => `sym.linkedClass`
 * `definitions` => `ctx.definitions`

### Member Lookup ###
`tpe.member(name)` and `tpe.decl(name)` now return a `Denotation`, not a
`Symbol`. If no definition is found they return `NoDenotation` (instead of
`NoSymbol`).

### Symbol Properties ###
Most `sym.isProperty` methods don't exist in dotc, test for flags instead. See
[dotc vs scalac: Trees, Symbols, Types & Denotations]

### Logging, Error Reporting, Failures ###

There are various kinds of logging:
  * Errors, warnings, etc: `ctx.inform`, `ctx.warning`, `ctx.error`, ...
  * Log messages displayed under `-Ylog:phase`: `log(msg)` in scalac =>
    `ctx.log(msg)` in dotc
  * Debug-Log messages displayed under `-Ydebug -Ylog:<phase>`: `debuglog(msg)`
    in scalac => `ctx.debuglog(msg)` in dotc
  * Assertions: `assert(invariant)`
  * Fatal errors: `abort(msg)` in scalac => `throw new
    dotty.tools.dotc.FatalError(msg)` in dotc


#### During development / debugging ####
Instead of `Predef.println`, use `dotc.config.Printers.somePrinter.println`
[Printers.scala]. Printers can be easily added, enabled and disabled
without changing command line arguments.

```scala
val default: Printer = new Printer // new Printer => print
val core:    Printer = noPrinter   // noPrinter   => shut up
```

[dotc vs scalac: Trees, Symbols, Types & Denotations]: https://github.com/lampepfl/dotty/wiki/dotc-vs-scalac:-Trees,-Symbols,-Types-&-Denotations
[Printers.scala]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/config/Printers.scala
