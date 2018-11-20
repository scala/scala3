---
layout: doc-page
title: "Replacing Implicits"
---

The previous two pages proposed high-level syntax for implicit definitions and a new syntax for implicit parameters.

This addresses all the issues mentioned in the [Motivation](./motivation.md), but it leaves us with two related constructs: new style witnesses and context parameters and traditional implicits. This page discusses what would be needed to get rid of `implicit` entirely.

## Abstract and Alias Witnesses

Witness definitions can be abstract.
As an example for an abstract witness consider the following fragment that's derived from Scala's Tasty extractor framework:
```scala
trait TastyAPI {
  type Symbol
  trait SymDeco {
    def (sym: Symbol) name: Name
    def (sym: Symbol) tpe: Type
  }
  witness symDeco: SymDeco
}
```
Here, `symDeco` is available as a witness for the `SymDeco` trait but its actual implementation
is deferred to subclasses of the `TastyAPI` trait.

An example of an alias witness would be an implementation of `symDeco` in terms of some internal compiler structure:
```scala
trait TastyImpl extends TastyAPI {
  witness symDeco: SymDeco = compilerSymOps
}
```
Note that the result type of an abstract or alias witness is introduced with a colon instead of a `for`. This seems more natural since it evokes the similarity to implicit parameters, whose type is also given following a `:`. It also avoids the syntactic ambiguity with a witness
for a class that does not add any new definitions. I.e.
```scala
witness a for C   // concrete witness for class C, no definitions added
witness b: C      // abstract witness for class C
```
Further examples of alias witnesses:
```scala
witness ctx = outer.ctx
witness ctx: Context = outer.ctx
witness byNameCtx with (): Context = outer.ctx
witness f[T]: C[T] = new C[T]
witness g with (ctx: Context): D = new D(ctx)
```
As another example, if one had already defined classes `IntOrd` and `ListOrd`, witnesses for them could be defined as follows:
```scala
class IntOrd extends Ord[Int] { ... }
class ListOrd[T: Ord] extends Ord[List[T]] { ... }

witness intOrd: Ord[Int] = new IntOrd
witness listOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]
```
The result type of a alias witness is mandatory unless the witness definition
occurs as a statement in a block and lacks any type or value parameters. This corresponds to the same restriction for implicit vals in Dotty.

Abstract witnesses are equivalent to abstract implicit defs. Alias witnesses are equivalent to implicit defs if they are parameterized or have a `=> T` result type.
They translate to implicit vals otherwise. For instance, the witnesses defined so far in this section are equivalent to:
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
The `lazy` modifier is applicable to unparameterized alias witnesses. If present, the resulting implicit val is lazy. For instance,
```scala
lazy witness intOrd2: Ord[Int] = new IntOrd
```
would be equivalent to
```scala
lazy implicit val intOrd2: Ord[Int] = new IntOrd
```

## Implicit Conversions and Classes

The only use cases that are not yet covered by the proposal are implicit conversions and implicit classes. We do not propose to use `witness` in place of `implicit` for these, since that would bring back the uncomfortable similarity between implicit conversions and parameterized implicit aliases. However, there is a way to drop implicit conversions entirely. Scala 3 already [defines](https://github.com/lampepfl/dotty/pull/2065) a class `ImplicitConverter` whose instances are available as implicit conversions.
```scala
  abstract class ImplicitConverter[-T, +U] extends Function1[T, U]
```
One can define all implicit conversions as witnesses of this class. E.g.
```scala
witness StringToToken for ImplicitConverter[String, Token] {
  def apply(str: String): Token = new KeyWord(str)
}
```
The fact that this syntax is more verbose than simple implicit defs could be a welcome side effect since it might dampen any over-enthusiasm for defining implicit conversions.

That leaves implicit classes. Most use cases of implicit classes are probably already covered by extension methods. For the others, one could always fall back to a pair of a regular class and an `ImplicitConverter` witness. It would be good to do a survey to find out how many classes would be affected.

## Summoning a Witness

Besides `implicit`, there is also `implicitly`, a method defined in `Predef` that computes an implicit value for a given type. Keeping with the "witness" terminology, it seems apt to introduce the name `summon` for this operation. So `summon[T]` summons a witness for `T`, in the same way as `implicitly[T]` did. The definition of `summon` is straightforward:
```scala
def summon[T] with (x: T) = x
```

## Syntax

The syntax changes for this page are summarized as follows:
```
WitnessDef      ::=  ...
                  |  id WitnessParams ‘:’ Type ‘=’ Expr
                  |  id ‘:’ ‘=>’ Type ‘=’ Expr
                  |  id ‘=’ Expr
```
In addition, the `implicit` modifier is removed together with all [productions]((http://dotty.epfl.ch/docs/internals/syntax.html) that reference it.
