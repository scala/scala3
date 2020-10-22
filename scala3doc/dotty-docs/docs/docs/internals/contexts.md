---
layout: doc-page
title: Contexts
---

The `Context` contains the state of the compiler, for example

  * `settings`
  * `freshNames` (`FreshNameCreator`)
  * `period` (run and phase id)
  * `compilationUnit`
  * `phase`
  * `tree` (current tree)
  * `typer` (current typer)
  * `mode` (type checking mode)
  * `typerState` (for example undetermined type variables)
  * ...

### Contexts in the typer ###
The type checker passes contexts through all methods and adapts fields where
necessary, e.g.

```scala
case tree: untpd.Block => typedBlock(desugar.block(tree), pt)(ctx.fresh.withNewScope)
```

A number of fields in the context are typer-specific (`mode`, `typerState`).

### In other phases ###
Other phases need a context for many things, for example to access the
denotation of a symbols (depends on the period). However they typically don't
need to modify / extend the context while traversing the AST. For these phases
the context can be simply an implicit class parameter that is then available in
all members.

**Careful**: beware of memory leaks. Don't hold on to contexts in long lived
objects.

### Using contexts ###
Nested contexts should be named `ctx` to enable implicit shadowing:

```scala
scala> class A

scala> def foo(implicit a: A) { def bar(implicit b: A) { println(implicitly[A]) } }
<console>:8: error: ambiguous implicit values:
 both value a of type A
 and value b of type A
 match expected type A
       def foo(implicit a: A) { def bar(implicit b: A) { println(implicitly[A]) } }

scala> def foo(implicit a: A) { def bar(implicit a: A) { println(implicitly[A]) } }
foo: (implicit a: A)Unit
```
