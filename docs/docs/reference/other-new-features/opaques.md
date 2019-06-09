---
layout: doc-page
title: "Opaque Type Aliases"
---

Opaque types aliases provide type abstraction without any overhead. Example:

```scala
object Logarithms {

  opaque type Logarithm = Double

  object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None
  }

  // Extension methods define opaque types' public APIs
  delegate LogarithmOps {
    def (x: Logarithm) toDouble: Double = math.exp(x)
    def (x: Logarithm) + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def (x: Logarithm) * (y: Logarithm): Logarithm = Logarithm(x + y)
  }
}
```

This introduces `Logarithm` as a new type, which is implemented as `Double` but is different from it. The fact that `Logarithm` is the same as `Double` is only known in the scope where
`Logarithm` is defined which in this case is object `Logarithms`.

The public API of `Logarithm` consists of the `apply` and `safe` methods that convert from doubles to `Logarithm` values, an extension method `toDouble` that converts the other way,
and operations `+` and `*` on logarithm values. The implementations of these functions
type-check because within object `Logarithms`, the type `Logarithm` is just an alias of `Double`.

Outside its scope, `Logarithm` is treated as a new abstract type. So the
following operations would be valid because they use functionality implemented in the `Logarithm` object.

```scala
  import Logarithms._
  import Predef.{any2stringadd => _, _}

  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
```

But the following operations would lead to type errors:

```scala
  val d: Double = l       // error: found: Logarithm, required: Double
  val l2: Logarithm = 1.0 // error: found: Double, required: Logarithm
  l * 2                   // error: found: Int(2), required: Logarithm
  l / l2                  // error: `/` is not a member fo Logarithm
```

Aside: the `any2stringadd => _` import suppression is necessary since otherwise the universal `+` operation in `Predef` would take precedence over the `+` extension method in `LogarithmOps`. We plan to resolve this wart by eliminating `any2stringadd`.

### Bounds For Opaque Type Aliases

Opaque type aliases can also come with bounds. Example:
```scala
object Access {

  opaque type Permissions = Int
  opaque type PermissionChoice = Int
  opaque type Permission <: Permissions & PermissionChoice = Int

  def (x: Permissions) & (y: Permissions): Permissions = x & y
  def (x: PermissionChoice) | (y: PermissionChoice): PermissionChoice = x | y
  def (x: Permissions) is (y: Permissions) = (x & y) == y
  def (x: Permissions) isOneOf (y: PermissionChoice) = (x & y) != 0

  val NoPermission: Permission = 0
  val ReadOnly: Permission = 1
  val WriteOnly: Permission = 2
  val ReadWrite: Permissions = ReadOnly & WriteOnly
  val ReadOrWrite: PermissionChoice = ReadOnly | WriteOnly
}
```
The `Access` object defines three opaque types:

 - `Permission`, representing a single permission,
 - `Permissions`, representing a conjunction (logical "and") of permissions,
 - `PermissionChoice`, representing a disjunction (logical "or") of permissions.

All three opaque types have the same underlying representation type `Int`. The
`Permission` type has an upper bound `Permissions & PermissionChoice`. This makes
it known outside the `Access` object that `Permission` is a subtype of the other
two types.  Hence, the following usage scenario type-checks.
```scala
object User {
  import Access._

  case class Item(rights: Permissions)

  val x = Item(ReadOnly)  // OK, since Permission <: Permissions

  assert( x.rights.is(ReadWrite) == false )
  assert( x.rights.isOneOf(ReadOrWrite) == true )
}
```
On the other hand, the call `x.rights.isOneOf(ReadWrite)` would give a type error
since `Permissions` and `PermissionChoice` are different, unrelated types outside `Access`.

[More details](opaques-details.md)
