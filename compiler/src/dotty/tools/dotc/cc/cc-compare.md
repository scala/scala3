# Capture Set Comparison

## Capture reference definition

All references in capture sets are `CaptureRef`s.

Valid `CaptureRef`s can be categorized into two kinds:

1. Value references, representing some singleton values in the set:

  * A singleton reference `SingletonCaptureRef`, e.g. `x`, `x.y`, `this`
    - Stable `TermRef`
    - `TermParamRef`
    - `ThisType`

    The info of a value reference can be:

    * `CapturingType(parent, refs)`, a type with capture set: `x: A^{a, b}`;

    * a value reference: `x: (y.type & A)`. In this case, `x` and `y` should be treated as the same reference during comparison.

  * A union `|` of two value references, e.g. `x | y | z.a`

  * An intersection `&` containing at least one value reference, e.g. `x & y & A`

  * `MaybeCapability(x)` or `ReachCapability(x)` where `x` is a value reference.

2. Capture variable references, representing capture set polymorphism.
  All capture variable references are bounded by `CapSet`.
  A capture variable should not have any value instance, e.g. `val x: CapSet^{a}`

  * A type reference, e.g. `C` in `{x, C}`

    - `TypeRef`

    - `TypeParamRef`

    The info of a capture variable reference can be:

    * a `TypeBounds(lo, hi)` where `lo` and `hi` are both capture variable references.
      If the type is an alias, both `lo` and `hi` are the same type.

    * a `CapturingType(parent, refs)`, where `parent` is a capture variable reference.

  * A union or intersection of two capture variable references, e.g. `C | CapSet^{x, y}`

## Comparison

Capture set comparison starts at:

```scala
class CaptureSet:
  final def subCaptures(that: CaptureSet, frozen: Boolean)(using Context): CompareResult
```

Later, `def subsumes(y: CaptureRef)` is called to compare two capture references: `{y} <:< {this}`.

There are three cases when comparing two capture references:

1. Both are value references: the comparison should be done in the `subsumes`.

2. Both are capture variable references: the subtyping should be used to compare the two types.
  The existing subtyping algorithm is able to check all possible type relations.

3. One is a value reference and the other is a capture variable reference.
  This is the most tricky case.
  The value reference should be converted to a capture variable with the reference in the capture set.

  For example, when comparing `{x} <:< {C}`, where `C` is defined as `cap C >: {x} <: C2`,
  the comparison becomes `CapSet^{x} <:< C`.
  By the subtyping algorithm, `CapSet^{x}` will be compared with the lower bound of `C`, which is `{x}`.
  Then the comparison is reduced to `{x} <:< {x}`, which is true.

The tricky part is the info of `this`: for example, `x: IO^{io}` and `y: IO^{io}`, `{x} <:< {y}` should be false, as `x` and `y` can be two different instances both capturing `io`.

## Detailed comparison rules

### Subsuming `{y} <:< {x}`

Union and intersection types are treated specially in the rules,
as they are not allowed directly in the capture set, for example, `A^{(x.type | y.type), z}`.
Therefore, a value reference must be a singleton type, `SingletonCaptureRef`.
Note the `||` and `&&` in the cases, because we only consider **equivalent references**.

Assuming both `x` and `y` are valid `CaptureRef`s, `x.subsumes(y)`:

TODO: `subsumesExistentially` should be considered.

1. If `x` and `y` are the same reference, `x eq y`.
2. If `x` is a root capability.
3. If `y` is a singleton type and not a root capability,
  1. If `x` is also a singleton type, and `x =:= y`.
  2. If `y` is a selection `ypre.f`, and `x.subsumes(ypre)`.
  3. By the info of `y`,
    * If `y.info` is a singleton type, `x.subsumes(y.info)`.
    * If `y =:= y1 | y2` and `y1` and `y2` are both singleton types, `x.subsumes(y1) && x.subsumes(y2)`.
    * If `y =:= y1 & y2`, `x.subsumes(y1) || x.subsumes(y2)`.
  4. If `x` is bounded by `CapSet` and `CapSet^{y} <:< x`.
4. If `y` is a `MaybeCapability(y1)` and `x` is a `MaybeCapability(x1)`, `x1.subsumes(y1)`.
5. If `x` is a `ReachCapability(x1)` and `y` is a `ReachCapability(y1)`, `x1.subsumes(y1)`.
6. If `x` is a singleton type,
  1. By the info of `x`,
    * If `x.info` is a singleton type, `x.info.subsumes(y)`.
    * If `x =:= x1 | x2`, `x1.subsumes(y) && x2.subsumes(y)`.
    * If `x =:= x1 & x2`, `x1.subsumes(y) || x2.subsumes(y)`.
  2. If `y` is bounded by `CapSet` and `y <:< CapSet^{x}`.
7. If `x` and `y` are both bounded by `CapSet`, `y <:< x`.
8. If `x` is bounded by `CapSet` and `y` is assumed to be contained in `x`.


### Subtyping `T1 <:< T2`

`T1 <:< T2`:

TODO: we are still not sure about the rules for union and intersection types.
For example, should `C2^{x}` be a subtype of `C1^{x} | C2^{y}` given `x` and `y` are different references?
