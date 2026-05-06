---
layout: doc-page
title: "Target-Typed Companion Shorthand"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/target-typed-companion-shorthand.html
---

A leading `.` followed by an identifier in a target-typed position is shorthand
for `T.X`, where `T` is the expected type. The feature is enabled per-file with

```scala
import scala.language.experimental.targetTypedCompanionShorthand
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

With target-typed companion shorthand the parameters of `Shape.apply` provide the expected
types `Geometry` and `Color`, and `.Circle` / `.Red` resolve against those:

```scala
val s = Shape(.Circle, .Red)
```

## Where it fires

A `.id` form is recognised as target-typed companion shorthand when it appears at the start
of an expression or pattern — that is, after one of the tokens `(`, `,`, `=`,
`=>`, `case`, `then`, `else`, `{`, `[`, `using`, `;`, `do`, `yield`, `:`,
`return`, or at the start of a block. It does **not** fire after an existing
expression, where `.id` continues to denote selection on that expression
(method-chain continuation is preserved):

```scala
val s: String = "abc"
  .toUpperCase   // method chain — unchanged
  .reverse

val c: Color = .Red  // target-typed companion shorthand
```

## Patterns

The same shorthand works in pattern positions, where the expected type comes
from the scrutinee or extractor parameter:

```scala
shape match
  case Shape(.Triangle, .Red) => "warning"
  case Shape(.Circle,   .Red) => "stop"
  case _                      => "unknown"

color match
  case .Red | .Blue => "primary"
  case _            => "other"
```

## Resolution

`.X` desugars to `T.X` where `T` is the principal class component of the
expected type, after stripping prototype layers, dealiasing and dropping
dependent refinements. The selection is then re-typed by the regular `Select`
machinery, so:

- overload selection picks the alternative that admits a member named `X`;
- implicit conversions are inserted to bridge `T.X.type` to a wider expected
  type;
- the resolved member must not be an anonymous given (per SIP-80).

## Errors

The following are reported as compile errors:

- the leading-dot syntax is used without the language import;
- the expected type is undetermined (e.g., `def f[A](a: A); f(.Red)`);
- the target type has no companion module;
- the named member does not exist on the companion.

## Limitations

The following positions are not supported in this initial implementation.
Each has a simple workaround.

### Direct RHS of an infix operator

A leading-dot expression cannot appear directly as the right operand of an
infix operator, because the parser's infix-operand starter set does not
include `.`:

```scala
c |+| .Red          // parse error
c matchesColor .Red // parse error
c == .Red           // parse error
```

Workarounds:

```scala
c |+| (.Red)            // parens
c matchesColor (.Red)
c.matchesColor(.Red)    // method syntax

c == Color.Red          // built-in `==` has RHS expected type `Any`,
                        // so even `c == (.Red)` cannot resolve `.Red`;
                        // spell out `Color.Red` or pattern match.
```

### Same-arity overloads with disjoint companion members

When two overloads have the same arity and only differ in their parameter
type, the typer cannot yet resolve `.X` against each candidate's parameter
type. The user must disambiguate:

```scala
def f(c: Color): String
def f(d: Direction): String

f(.Red)             // error: expected type cannot be determined
f((.Red: Color))    // explicit ascription works
```

If the overloads have different arities, `narrowBySize` picks a single
candidate before SIP-80 logic runs and `.X` resolves against that
candidate's parameter type, so `f(.Red, 5)` works when only the 2-arg
overload matches by arity.

If the overloads agree on the parameter type at the relative-selection's
position — for example because one adds a default-valued parameter as a
binary-compatibility evolution — the shared formal is used and the call
resolves cleanly:

```scala
def bar(a: Animal): Unit                          = ???
def bar(a: Animal, b: Animal = null): Unit        = ???
bar(.Cat)   // ok — both overloads share `a: Animal`
```

### Bare `.` cursor in IDE completion

The presentation-compiler completer requires at least one identifier
character after `.` (`.r<TAB>`) — a bare `.<TAB>` does not parse and
therefore offers no SIP-80 suggestions. This matches Swift and Dart
behaviour.

## See also

- [SIP-80](https://github.com/scala/improvement-proposals)
