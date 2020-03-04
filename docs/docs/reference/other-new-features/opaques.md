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
  extension logarithmOps on (x: Logarithm) {
    def toDouble: Double = math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y
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

Aside: the `any2stringadd => _` import suppression is necessary since otherwise the universal `+` operation in `Predef` would take precedence over the `+` extension method in `logarithmOps`. We plan to resolve this wart by eliminating `any2stringadd`.

### Bounds For Opaque Type Aliases

Opaque type aliases can also come with bounds. Example:
```scala
object Access {

  opaque type Permissions = Int
  opaque type PermissionChoice = Int
  opaque type Permission <: Permissions & PermissionChoice = Int

  def (x: Permissions) & (y: Permissions): Permissions = x | y
  def (x: PermissionChoice) | (y: PermissionChoice): PermissionChoice = x | y
  def (granted: Permissions).is(required: Permissions) = (granted & required) == required
  def (granted: Permissions).isOneOf(required: PermissionChoice) = (granted & required) != 0

  val NoPermission: Permission = 0
  val Read: Permission = 1
  val Write: Permission = 2
  val ReadWrite: Permissions = Read | Write
  val ReadOrWrite: PermissionChoice = Read | Write
}
```
The `Access` object defines three opaque types:

 - `Permission`, representing a single permission,
 - `Permissions`, representing a set of permissions with the meaning "all of these permissions granted",
 - `PermissionChoice`, representing a set of permissions with the meaning "at least one of these permissions granted".

Outside the `Access` object, values of type `Permissions` may be combined using the `&` operator,
where `x & y` means "all permissions in `x` *and* in `y` granted".
Values of type `PermissionChoice` may be combined using the `|` operator,
where `x | y` means "a permission in `x` *or* in `y` granted".

Note that inside the `Access` object, the `&` and `|` operators always resolve to the corresponding methods of `Int`,
because members always take precedence over extension methods.
Because of that, the `|` extension method in `Access` does not cause infinite recursion.
Also, the definition of `ReadWrite` must use `|`,
even though an equivalent definition outside `Access` would use `&`.

All three opaque types have the same underlying representation type `Int`. The
`Permission` type has an upper bound `Permissions & PermissionChoice`. This makes
it known outside the `Access` object that `Permission` is a subtype of the other
two types.  Hence, the following usage scenario type-checks.
```scala
object User {
  import Access._

  case class Item(rights: Permissions)

  val roItem = Item(Read)  // OK, since Permission <: Permissions
  val rwItem = Item(ReadWrite)
  val noItem = Item(NoPermission)

  assert( roItem.rights.is(ReadWrite) == false )
  assert( roItem.rights.isOneOf(ReadOrWrite) == true )

  assert( rwItem.rights.is(ReadWrite) == true )
  assert( rwItem.rights.isOneOf(ReadOrWrite) == true )

  assert( noItem.rights.is(ReadWrite) == false )
  assert( noItem.rights.isOneOf(ReadOrWrite) == false )
}
```
On the other hand, the call `roItem.rights.isOneOf(ReadWrite)` would give a type error
since `Permissions` and `PermissionChoice` are different, unrelated types outside `Access`.

[More details](opaques-details.md)
