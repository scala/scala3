---
layout: doc-page
title: "Given Imports"
---

A special form of import is used to import given instances. Example:
```scala
object A {
  class TC
  given tc as TC
  def f where TC = ???
}
object B {
  import A._
  import given A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the given instance `tc`. Conversely, the second import `import given A._` will import _only_ that given instance.

Generally, a normal import clause brings definitions other than given instances into scope whereas a `given` import brings only given instances into scope.

There are two main benefits arising from these rules:

 - It is made clearer where givens in scope are coming from.
   In particular, it is not possible to hide imported givens in a long list of regular imports.
 - It enables importing all givens
   without importing anything else. This is particularly important since givens
   can be anonymous, so the usual recourse of using named imports is not
   practical.

### Importing By Type

Since givens can be anonymous it is not always practical to import them by their name, and wildcard imports are typically used instead. By-type imports provide a more specific alternative to wildcard imports, which makes it clearer what is imported. Example:

```scala
import given A.{_: TC}
```
This imports any given in `A` that has a type which conforms to `TC`. Importing givens of several types `T1,...,Tn`
is expressed by bounding with a union type.
```
import given A.{_: T1 | ... | Tn}
```
Importing all instances of a parameterized if expressed by wildcard arguments.
For instance, assuming the object
```scala
object Instances {
  given intOrd as Ordering[Int]
  given [T: Ordering] listOrd as Ordering[List[T]]
  given ec as ExecutionContext = ...
  given im as Monoid[Int]
}
```
the import
```scala
import given Instances.{_: Ordering[?] | ExecutionContext}
```
would import the `intOrd`, `listOrd`, and `ec` instances but leave out the `im` instance, since it fits none of the specified bounds.

By-type imports can be mixed with by-name imports. If both are present in an import clause, by-type imports come last. For instance, the import clause
```scala
import given Instances.{im, _: Ordering[?]}
```
would import `im`, `intOrd`, and `listOrd` but leave out `ec`.

Bounded wildcard selectors also work for normal imports and exports. For instance, consider the following `enum` definition:
```scala
enum Color {
  case Red, Green, Blue, Magenta

  def isPrimary(c: Color): Boolean = ...
}
export Color.{_: Color}
```
The export clause makes all four three `Color` values available as unqualified constants, but
leaves the `isPrimary` method alone.

### Migration

The rules for imports given above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to given instances and imports.

The following modifications avoid this hurdle to migration.

 1. An "import given" also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal import or by an import given.

 2. In Scala 3.1, old-style implicits accessed through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, old-style implicits accessed through a normal import
    will give a compiler error.

These rules mean that library users can use `import given` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
representation clauses once their user base has migrated.
