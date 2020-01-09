---
layout: doc-page
title: "Default Imports"
---

A special form of import wildcard selector is used to import default. Example:
```scala
object A {
  class TC
  default tc of TC
  def f(given TC) = ???
}
object B {
  import A._
  import A.{default _}
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the default `tc`. Conversely, the second import `import A.{default _}`
will import _only_ that default. The two import clauses can also be merged into one:
```scala
object B
  import A.{default _, _}
```

Generally, a normal wildcard selector `_` brings all definitions other than defaults or extensions into scope
whereas a `default _` selector brings all defaults (including those resulting from extensions) into scope.

There are two main benefits arising from these rules:

 - It is made clearer where defaults in scope are coming from.
   In particular, it is not possible to hide imported defaults in a long list of regular wildcard imports.
 - It enables importing all defaults
   without importing anything else. This is particularly important since defaults
   can be anonymous, so the usual recourse of using named imports is not
   practical.

### Importing By Type

Since defaults can be anonymous it is not always practical to import them by their name, and wildcard imports are typically used instead. By-type imports provide a more specific alternative to wildcard imports, which makes it clearer what is imported. Example:

```scala
import A.{default TC}
```
This imports any default in `A` that has a type which conforms to `TC`. Importing defaults of several types `T1,...,Tn`
is expressed by multiple `default` selectors.
```
import A.{default T1, ..., default Tn}
```
Importing all defaults of a parameterized type is expressed by wildcard arguments.
For instance, assuming the object
```scala
object Instances {
  default intOrd for Ordering[Int]
  default [T: Ordering] listOrd for Ordering[List[T]]
  default ec for ExecutionContext = ...
  default im for Monoid[Int]
}
```
the import
```scala
import Instances.{default Ordering[?], default ExecutionContext}
```
would import the `intOrd`, `listOrd`, and `ec` instances but leave out the `im` instance, since it fits none of the specified bounds.

By-type imports can be mixed with by-name imports. If both are present in an import clause, by-type imports come last. For instance, the import clause
```scala
import Instances.{im, default Ordering[?]}
```
would import `im`, `intOrd`, and `listOrd` but leave out `ec`.

<!--
Bounded wildcard selectors also work for normal imports and exports. For instance, consider the following `enum` definition:
```scala
enum Color {
  case Red, Green, Blue, Magenta

  def isPrimary(c: Color): Boolean = ...
}
export Color.{_: Color}
```
The export clause makes all four `Color` values available as unqualified constants, but
leaves the `isPrimary` method alone.
-->

### Migration

The rules for imports stated above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to defaults and default imports.

The following modifications avoid this hurdle to migration.

 1. A `default` import selector also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a `_` or a `default _` wildcard selector.

 2. In Scala 3.1, old-style implicits accessed through a `_` wildcard import will give a deprecation warning.

 3. In some version after 3.1, old-style implicits accessed through a `_` wildcard import will give a compiler error.

These rules mean that library users can use `default _` selectors to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
representation clauses once their user base has migrated.

### Syntax

```
Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
ImportExpr        ::=  StableId ‘.’ ImportSpec
ImportSpec        ::=  id
                    |  ‘_’
                    |  ‘{’ ImportSelectors) ‘}’
ImportSelectors   ::=  id [‘=>’ id | ‘=>’ ‘_’] [‘,’ ImportSelectors]
                    |  WildCardSelector {‘,’ WildCardSelector}
WildCardSelector  ::=  ‘_'
                    |  ‘default’ (‘_' | InfixType)
Export            ::=  ‘export’ ImportExpr {‘,’ ImportExpr}
```