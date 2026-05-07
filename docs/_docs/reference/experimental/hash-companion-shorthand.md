---
layout: doc-page
title: "`#` Companion Shorthand"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/hash-companion-shorthand.html
---

A leading `#` followed by an identifier in a target-typed position is shorthand
for `T.X`, where `T` is the expected type. The feature is enabled per-file with

```scala
import scala.language.experimental.hashCompanionShorthand
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

With the `#` companion shorthand the parameters of `Shape.apply` provide the
expected types `Geometry` and `Color`, and `#Circle` / `#Red` resolve against
those:

```scala
val s = Shape(#Circle, #Red)
```

## Where it fires

`#id` is recognised wherever it would otherwise be a syntax error — that is,
anywhere in expression or pattern position. There is no lexical-context gate
because `#` has no current meaning at expression position in Scala (its only
existing role is the type-level projection `T#X`, which the expression parser
never reaches).

## Patterns

The same shorthand works in pattern positions, where the expected type comes
from the scrutinee or extractor parameter:

```scala
shape match
  case Shape(#Triangle, #Red) => "warning"
  case Shape(#Circle,   #Red) => "stop"
  case _                      => "unknown"

color match
  case #Red | #Blue => "primary"
  case _            => "other"
```

## Resolution

`#X` desugars to `T.X` where `T` is the principal class component of the
expected type, after stripping prototype layers, dealiasing transparent
aliases, dropping dependent refinements, and **peeling `T | Null` (or
`Null | T`) to `T`**. The selection is then re-typed by the regular `Select`
machinery, so:

- overload selection picks the alternative that admits a member named `X`;
- implicit conversions are inserted to bridge `T.X.type` to a wider expected
  type;
- the resolved member must not be an anonymous given (per SIP-80).

## Errors

The following are reported as compile errors:

- the leading-`#` syntax is used without the language import;
- the expected type is undetermined (e.g. `def f[A](a: A); f(#Red)`);
- the target type has no companion module;
- the named member does not exist on the companion.

## Limitations

The following situations are not supported in this initial implementation;
each has a simple workaround.

### Equality and inequality

`==` and `!=` are defined on `Any`, so the right operand's expected type is
`Any`, which has no companion members named `Red`, `Blue`, etc. The shorthand
therefore does not help with equality:

```scala
c == #Red                 // ERROR — Any has no member Red.
c == Color.Red            // OK — the standard form.
```

This is a consequence of the resolution rule, not a special-case carve-out:
`==` simply does not propagate a useful expected type. Library authors who
want comparison-style use sites to participate in the shorthand can provide
typed methods (for example `def matches(other: Color): Boolean`).

### Same-arity overloads with disjoint companion members

When two overloads have the same arity and their parameter types at the
`#X` position differ, the typer cannot yet resolve `#X` against each
candidate's parameter type. The user disambiguates:

```scala
def f(c: Color): String
def f(d: Direction): String

f(#Red)             // error: expected type cannot be determined
f((#Red: Color))    // explicit ascription works
```

If the overloads have different arities, `narrowBySize` picks a single
candidate before SIP-80 logic runs and `#X` resolves against that
candidate's parameter type, so `f(#Red, 5)` works when only the 2-arg
overload matches by arity.

If the overloads agree on the parameter type at the `#X` position — for
example because one adds a default-valued parameter as a binary-compatibility
evolution — the shared formal is used and the call resolves cleanly:

```scala
def bar(a: Animal): Unit                    = ???
def bar(a: Animal, b: Animal = null): Unit  = ???
bar(#Cat)   // ok — both overloads share `a: Animal`
```

### Bare `#` cursor in IDE completion

The presentation-compiler completer requires at least one identifier
character after `#` (`#r<TAB>`) — a bare `#<TAB>` does not parse and
therefore offers no SIP-80 suggestions. This matches Swift and Dart
behaviour for their analogous shorthand.

## See also

- [SIP-80 — `#` Companion Shorthand](https://github.com/scala/improvement-proposals)
