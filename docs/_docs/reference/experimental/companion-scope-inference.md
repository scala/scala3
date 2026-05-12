---
layout: doc-page
title: "Companion Scope Inference"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/companion-inference.html
---

When a bare identifier cannot be resolved through normal name lookup and the
position has a known expected type `T`, the compiler retries the lookup
against `T`'s companion module. The feature is enabled per-file with

```scala
import scala.language.experimental.companionScopeInference
```

## Motivation

Hierarchical sealed/enum ADTs often require fully qualifying their constructors:

```scala
final case class Shape(geometry: Shape.Geometry, color: Shape.Color)
object Shape:
  sealed trait Geometry
  object Geometry:
    case object Triangle extends Geometry
    case object Circle   extends Geometry
  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

val s = Shape(Shape.Geometry.Circle, Shape.Color.Red)
```

With companion scope inference the parameters of `Shape.apply` provide the
expected types `Geometry` and `Color`, and the bare identifiers `Circle` /
`Red` are looked up in their companions:

```scala
val s = Shape(Circle, Red)
```

## How resolution works

Companion inference is a *fallback* in name resolution; it never changes
the meaning of any program that compiles today:

1. **Try normal name lookup.** Lexical scope, imports, exports, package
   members, inherited members, etc. If a unique result is found, use it.
2. **If normal lookup found nothing**, and the position has a known
   expected type `T`, reduce `T`:
    - strip prototype layers,
    - dealias transparent aliases,
    - drop dependent refinements,
    - take the principal class component (for refined/intersection types),
    - drop `Null` arms from `T | Null` (recursively).
3. **Look up the identifier** as a term-level member of the reduced type's
   companion object. Opaque type aliases use their own companion.
4. **Re-type** the desugared `T.X` through the usual `Select` machinery,
   so implicit conversions, overload resolution, and pattern-mode handling
   all apply.

## Patterns

In a pattern position the same fallback fires. Lower-case identifiers
still bind fresh variables (existing Scala rule); capitalised identifiers
resolve as stable references and so can fall back to the companion:

```scala
val c: Color = ???
c match
  case Red | Blue => "primary"
  case Green      => "secondary"
```

## Examples

### Chaining

When the expected type's companion holds an *intermediate* type, only the
leftmost identifier uses inference; subsequent segments are ordinary path
selection on the resulting value:

```scala
sealed trait Animal
object Animal:
  sealed trait Mammal extends Animal
  object Mammal:
    case object Dog extends Mammal
    case object Cat extends Mammal

def describe(a: Animal): String = ???
describe(Mammal.Dog)   // Mammal → Animal.Mammal; then .Dog
```

### Aliases, imports, and exports

The dealiasing step makes transparent aliases / `import` / `export`
participate transparently:

```scala
object Lib:
  sealed trait Color
  object Color:
    case object Red extends Color

type MyColor = Lib.Color
val c: MyColor = Red       // dealias MyColor → Lib.Color → Lib.Color.Red
```

### Opaque types

From outside the defining module the alias is abstract; companion lookup
uses the alias's *own* companion:

```scala
object Lib:
  enum Severity:
    case INFO, WARN, ERROR
  opaque type Level = Severity
  object Level:
    val Info: Level = Severity.INFO

import Lib.Level
val l: Level = Info        // Lib.Level.Info, not Lib.Severity.INFO
```

### Overloads with default arguments

When two overloads agree on the parameter type at the unresolved
identifier's position — for instance because one is the binary-compatible
evolution of the other — companion scope inference fires against that shared
type and normal overload selection disambiguates by the other arguments:

```scala
def bar(a: Animal): Unit                    = ???
def bar(a: Animal, b: Animal = null): Unit  = ???

bar(Cat)                  // both overloads share Animal at position 0
```

## Limitations

The following situations are not supported in this initial implementation;
each has a simple workaround.

### Equality and inequality

`==` and `!=` are defined on `Any`, so the right operand's expected type
is `Any`, which has no useful companion. Companion inference does not help
with equality:

```scala
c == Red                  // ERROR — Any has no member Red.
c == Color.Red            // OK — the standard form.
```

### Same-arity overloads with disjoint companion members

When two overloads have the same arity and their parameter types at the
unresolved identifier's position differ, companion scope inference cannot decide
which expected type to use. The user disambiguates:

```scala
def foo(a: Animal): Unit = ???
def foo(c: Color):  Unit = ???

foo(Red)                  // ERROR
foo((Red: Color))         // OK — ascription pins expected type.
```

### Silent shadowing

Because there is no surface marker, a local of unrelated type that shares
a name with a desired companion member wins normal resolution. The result
is a *type-mismatch* error rather than a "not found" error:

```scala
val Triangle = "the letter T"
val s: Shape = Shape(Triangle, Red)   // type mismatch — Triangle is String
```

The same case applies to types imported by Predef. `Error` is in scope
everywhere (as `java.lang.Error`); if a companion member of the same name
is what was intended, the user qualifies (or chooses a different member
name).

### Polymorphic inference

Companion inference consumes an established expected type but does not
contribute back to type inference. Generic calls whose type parameter is
otherwise unconstrained still need an explicit type argument:

```scala
Seq[Color](Red, Blue)               // OK — explicit type argument.
val xs: Seq[Color] = Seq(Red, Blue) // OK — outer expected type pins A.
Seq(Red, Blue)                      // ERROR — A unconstrained.
```

## See also

- [SIP-80 — Companion Scope Inference](https://github.com/scala/improvement-proposals) (sibling of the `#X` draft on the same SIP number)
