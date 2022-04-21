# GADTs - Broad overview

There are multiple levels to the implementation. They deal with slightly different problems. The most important levels are the following ones:

1.  Figuring out which relationships are necessary (PatternTypeConstrainer)
2.  Breaking down the relationships (TypeComparer)
3.  Recording and bookkeeping of relationships (GadtConstraint)
4.  Looking up the information (TypeComparer, GadtConstraint)

Out of the levels above, 4. is by far the simplest one.

There are also other parts to supporting GADTs. Roughly in order of importance, they are:

1.  At specific points, abstract types are added to GadtConstraint. For instance, when entering a method, we add all type parameters to GadtConstraint.
2.  Casts need to be inserted when GADTs were used for type comparison.
    1.  `TypeComparer` keeps track of whether a GADT constraint was used in a mutable variable `usedGadt`.
3.  GADT constraints need to be stored in attachments to be used in PostTyper.
    1.  Attachment key is named `inferredGadtConstraints`.
4.  When we select members on a type that may have GADT constraints, we perform special "healing" by approximating the type using those constraints. We cannot take the constraints into account because member lookup is cached, and GADT constraints are only valid for specific scopes.

# Useful widgets

## Expr

This is the classical GADT example:

```scala
enum Expr[T] {
  case IntLit(value: Int) extends Expr[Int]
  case BoolLit(value: Boolean) extends Expr[Boolean]
  case IfExpr(
    cond: Expr[Boolean],
    when: Expr[T],
    otherwise: Expr[T],
  )
}
```

## EQ

The following enum will result in an equality constraint between `S` and `T` if we match on it:

```scala
enum EQ[S, T] {
  case Refl[U]() extends EQ[U, U]
}
```

## SUB

The following enum will result in a subtyping constraint `S <: T` if we match on it:

```scala
enum SUB[-S, +T] {
  case Refl[U]() extends SUB[U, U]
}
```

# Details of above

## What abstract types can have GADT constraints

Right now, we record GADT constraints for:

-   function/method type parameters
-   class type parameters

There is a branch on the way which will also record them for type members (so path-dependent types) and singleton types. It has a paper associated: "Implementing path-depepdent GADTs for Scala 3".

## What are necessary relationships? Any examples?

### Covariance means no constraint is necessary

Standard (non-case) classes allow "strange" inheritance which means that we cannot infer any information from covariant type parameters.

```scala
class Expr[+A]
class IntList extends Expr[List[Int]]

def foo[T](e: Expr[List[T]]): T =
  e match {
    case _ : IntList =>
      // e : Expr[List[Int]]
      // T <: Int
      0
  }

class Weird(list: List[String]) extends IntList with Expr[Nothing]
```

Case classes have a special check which disallows inheritance like `Weird`. This means we can infer extra information from them.

## Breaking down the constraints

```scala
class Expr[A]
class IntList extends Expr[List[Int]]

def foo[T](e: Expr[List[T]]): T =
  e match {
    case _ : IntList =>
      // Level 1:
      //   We start with e : Expr[List[T]]
      //   We check that e : IntList <: Expr[List[Int]
      //   Expr is invariant,
      //     so we have List[Int] <: List[T] , List[T] <: List[Int]
      // Level 2:
      //   We compare List[Int] <: List[T]
      //   We record  Int <: T
      //   We compare List[T] <: List[Int]
      //   We record  T <: Int
      0
  }
```

## Relation betweeen GadtConstraint and OrderingConstraint

### Internal and external types

GadtConstraint uses OrderingConstraint as the datastructure to record information about GADT constraints.

OrderingConstraint only supports working with TypeParamRefs.

GadtConstraint wants to record information about things other than TypeParamRefs.

To solve this, GadtConstraint internally creates TypeParamRefs which it adds to the OrderingConstraint it keeps internally. Whenever a GADT constraint is added, we "internalize" the type by replacing all the external types with the internal TypeParamRefs. Whenever we take bound information out of the GADT constraint, we need to "externalize" the types by replacing the internal TypeParamRefs with their external versions. To implement this, GadtConstraint keeps a bidirectional mapping between the external types and the internal TypeParamRefs.

The TypeParamRefs and TypeVars registered in one constraint cannot ever be present in types mentioned in the other type constraint. The internal TypeParamRefs and TypeVars cannot ever leak out of the GadtConstraint. We cannot ever record a bound in GadtConstraint which mentions TypeParamRefs used for type inference. (That part is ensured by the way TypeComparer is organised &#x2013; we will always try to record bounds in the "normal" constraint before recording a GADT bound.)

# Other details

## TypeComparer approximations

TypeComparer sometimes approximates the types it compares. Let's see an example based on these definitions:

```scala
class Expr[T]
class IntList extends Expr[Int]
```

when comparing if `IntList <: Expr[Int]`, `TypeComparer` will approximate `IntList` to `Expr[Int]`. Then it will compare `Expr[Int] <: Expr[Int]` with appropriate variables set.

The variables which TypeComparer sets are `approxState` and `frozenGadt`.

## Necessary/sufficient either

TypeComparer sometimes needs to approximate some constraints, specifically when dealing with intersection and union types. The way this approximation works changes if we're currently inferring GADT constraints. This is hopefully documented well in TypeComparer in doc comments for `necessaryEither` and `sufficientEither`.

## Types bound in patterns

```scala
(list : List[Int]) match {
  case lst : List[a] =>
    // a is a new type bound in the pattern.
    // We do not record any information about a.
    // We should know that a <: Int.
    // (Or it's fine to just have a =:= Int.)
    // We would not have this issue if we used a custom unapply.
    // Type case patterns create a fresh symbol even if they shouldn't.
    // (See indexPattern in Typer.)
}
```

## Internal structure of OrderingConstraint

Imagine we have two type parameters in scope, `A` and `B`.

We could record the following in `ctx.gadt`:

```text
A <: Int
B <: A
```

Then, we expect that calling ``ctx.gadt.bounds(`B`)`` will return `` `<: Int` ``.

In order to handle this, `GadtConstraint` relies on `OrderingConstraint`. Internally, it will represent the above constraints as follows:

```text
A <: Int
B <: Int
B <: A
```

The first two constraints are "entries" &#x2013; they are easy to look up whenever we ask for bounds of `A` or `B`. The third constraint is an ordering &#x2013; it helps with correctly propagating the bounds we record.

# Possible broad improvements

## Allow OrderingConstraint to record bounds for things other than TypeParamRefs

This would mean we no longer need to keep the bidirectional mapping in GadtConstraint.

## Not mixing OrderingConstraint and ConstraintHandling in GadtConstraint

GadtConstraint right now mixes OrderingConstraint and ConstraintHandling. The first one is supposed to be the immutable constraint datastructure. The second one implements mutable functionality around a variable containing the immutable datastructure.

GadtConstraint mixes them both. Things would be better organised if GadtConstraint was split like the normal constraint.

## Creating a separate TypeComparer for breaking down types into GADT constraints

TypeComparer is biased towards one specific way of approximating constraints. When we infer types, it's ok to be "optimistic". When inferring GADT constraints, we should be as pessimistic as possible, in order to only infer constraints which are necessary.

TypeComparer was originally written to support type inference and GADTs were only added later on. This means that the "default" way TypeComparer approximates wasn't even noticeable before, but when inferring GADT constraints could result in inferring unsound information.

We could potentially fix this by creating a special TypeComparer which would *only* "break down" subtype relationships to record GADT constraints.
