---
layout: doc-page
title: "Replacing Implicits"
---

The previous two pages proposed high-level syntax for implicit definitions and a new syntax for implicit parameters.

This addresses all the issues mentioned in the [Motivation](./motivation.md), but it leaves us with two related constructs: new style instance definitions and context parameters and traditional implicits. This page discusses what would be needed to get rid of `implicit` entirely.

## Abstract and Alias Instances

Instance definitions can be abstract.
As an example of an abstract instance, consider the following fragment that's derived from Scala's Tasty extractor framework:
```scala
trait TastyAPI {
  type Symbol
  trait SymDeco {
    def (sym: Symbol) name: Name
    def (sym: Symbol) tpe: Type
  }
  instance symDeco: SymDeco
}
```
Here, `symDeco` is available as a instance of the `SymDeco` trait but its actual implementation
is deferred to subclasses of the `TastyAPI` trait.

An example of an alias instance would be an implementation of `symDeco` in terms of some internal compiler structure:
```scala
trait TastyImpl extends TastyAPI {
  instance symDeco: SymDeco = compilerSymOps
}
```
Note that the result type of an abstract or alias instance is introduced with a colon instead of an `of`. This seems more natural since it evokes the similarity to implicit parameters, whose type is also given following a `:`. It also avoids the syntactic ambiguity with an instance definition of a class that does not add any new definitions. I.e.
```scala
instance a of C   // concrete instance of class C, no definitions added
instance b: C     // abstract instance of class C
```
Further examples of alias instances:
```scala
instance ctx = outer.ctx
instance ctx: Context = outer.ctx
instance byNameCtx with (): Context = outer.ctx
instance f[T]: C[T] = new C[T]
instance g with (ctx: Context): D = new D(ctx)
```
As another example, if one had already defined classes `IntOrd` and `ListOrd`, instances for them could be defined as follows:
```scala
class IntOrd extends Ord[Int] { ... }
class ListOrd[T: Ord] extends Ord[List[T]] { ... }

instance intOrd: Ord[Int] = new IntOrd
instance listOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]
```
The result type of a alias instance is mandatory unless the instance definition
occurs as a statement in a block and lacks any type or value parameters. This corresponds to the same restriction for implicit vals in Dotty.

Abstract instances are equivalent to abstract implicit defs. Alias instances are equivalent to implicit defs if they are parameterized or to implicit vals otherwise. For instance, the instances defined so far in this section are equivalent to:
```scala
implicit def symDeco: SymDeco
implicit val symDeco: SymDeco = compilerSymOps

implicit val ctx = outer.ctx
implicit val ctx: Context = outer.ctx
implicit def byNameCtx: Ctx = outer.ctx
implicit def f[T]: C[T] = new C[T]
implicit def g(implicit ctx: Context): D = new D(ctx)

implicit val intOrd: Ord[Int] = new IntOrd
implicit def listOrd(implicit ev: Ord[T]): Ord[List[T]] = new ListOrd[T]
```
The `lazy` modifier is applicable to unparameterized alias instances. If present, the resulting implicit val is lazy. For instance,
```scala
lazy instance intOrd2: Ord[Int] = new IntOrd
```
would be equivalent to
```scala
lazy implicit val intOrd2: Ord[Int] = new IntOrd
```

## Implicit Conversions and Classes

The only use cases that are not yet covered by the proposal are implicit conversions and implicit classes. We do not propose to use `instance` in place of `implicit` for these, since that would bring back the uncomfortable similarity between implicit conversions and parameterized implicit aliases. However, there is a way to drop implicit conversions entirely. Scala 3 already [defines](https://github.com/lampepfl/dotty/pull/2065) a class `ImplicitConversion` whose instances are available as implicit conversions.
```scala
  abstract class ImplicitConversion[-T, +U] extends Function1[T, U]
```
One can define all implicit conversions as instances of this class. E.g.
```scala
instance StringToToken of ImplicitConversion[String, Token] {
  def apply(str: String): Token = new KeyWord(str)
}
```
The fact that this syntax is more verbose than simple implicit defs could be a welcome side effect since it might dampen any over-enthusiasm for defining implicit conversions.

That leaves implicit classes. Most use cases of implicit classes are probably already covered by extension methods. For the others, one could always fall back to a pair of a regular class and an `ImplicitConversion` instance. It would be good to do a survey to find out how many classes would be affected.

## Summoning an Instance

Besides `implicit`, there is also `implicitly`, a method defined in `Predef` that computes an implicit value for a given type. A possible replacement could be `instanceOf`. Or, keeping with common usage, one could introduce the name `summon` for this operation. So `summon[T]` summons an instance of `T`, in the same way as `implicitly[T]` did. The definition of `summon` is straightforward:
```scala
def summon[T] with (x: T) = x
```

## Syntax

The syntax changes for this page are summarized as follows:
```
InstanceDef     ::=  ...
                  |  id InstanceParams ‘:’ Type ‘=’ Expr
                  |  id ‘=’ Expr
```
In addition, the `implicit` modifier is removed together with all [productions]((http://dotty.epfl.ch/docs/internals/syntax.html) that reference it.
