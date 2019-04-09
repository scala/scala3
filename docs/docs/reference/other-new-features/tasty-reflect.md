---
layout: doc-page
title: "TASTy Reflect"
---

TASTy Reflect enables inspection and construction of Typed Abstract Syntax Trees (TAST).
It may be used on quoted expressions (`quoted.Expr`) and quoted types (`quoted.Type`) from [Principled Meta-programming](./principled-meta-programming.html)
or on full TASTy files.

If you are writing macros, please first read [Principled Meta-programming](./principled-meta-programming.html).
You may find all you need without using TASTy Reflect.


## From quotes and splices to TASTs Reflect trees and back

`quoted.Expr` and `quoted.Type` are only meant for generative meta-programming, generation of code without inspecting the ASTs.
[Principled Meta-programming](./principled-meta-programming.html) provides the guarantee that the generation of code will be type-correct.
Using TASTy Reflect will break these guarantees and may fail at macro expansion time, hence additional explicit check must be done.


To provide reflection capabilities in macros we need to add an implicit parameter of type `scala.tasty.Reflection` and import it in the scope where it is used.

```scala
import scala.quoted._
import scala.tasty._

inline def natConst(x: => Int): Int = ~natConstImpl('(x))

def natConstImpl(x: Expr[Int])(implicit reflection: Reflection): Expr[Int] = {
  import reflection._
  ...
}
```

`import reflection._` will provide an `unseal` extension method on `quoted.Expr` and `quoted.Type` which returns a `reflection.Term` and `reflection.TypeTree` respectively.
It will also import all extractors and methods on TASTy Reflect trees. For example the `Term.Literal(_)` extractor used below.

```scala
def natConstImpl(x: Expr[Int])(implicit reflection: Reflection): Expr[Int] = {
  import reflection._
  val xTree: Term = x.unseal
  xTree match {
    case Term.Literal(Constant.Int(n)) =>
      if (n <= 0)
        throw new QuoteError("Parameter must be natural number")
      n.toExpr
    case _ =>
      throw new QuoteError("Parameter must be a known constant")
  }
}
```

To easily know which extractors are needed, the `reflection.Term.show` method returns the string representation of the extractors.

The method `reflection.Term.reify[T]` provides a way to go back to a `quoted.Expr`.
Note that the type must be set explicitly and that if it does not conform to it an exception will be thrown.
In the code above we could have replaced `n.toExpr` by `xTree.reify[Int]`.


## Inspect a TASTy file

To inspect the TASTy Reflect trees of a TASTy file a consumer can be defined in the following way.

```scala
class Consumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    // Do somthing with the tree
  }
}
```

Then the consumer can be instantiated with the following code to get the tree of the class `foo.Bar` for a foo in the classpath.

```scala
object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("foo.Bar"), new Consumer)
  }
}
```

## TASTy Reflect API

TASTy Reflect provides the following types:

```none
+- Tree -+- PackageClause
         +- Import
         +- Statement -+- Definition --+- PackageDef
         |             |               +- ClassDef
         |             |               +- TypeDef
         |             |               +- DefDef
         |             |               +- ValDef
         |             |
         |             +- Term --------+- Ident
         |                             +- Select
         |                             +- Literal
         |                             +- This
         |                             +- New
         |                             +- NamedArg
         |                             +- Apply
         |                             +- TypeApply
         |                             +- Super
         |                             +- Typed
         |                             +- Assign
         |                             +- Block
         |                             +- Lambda
         |                             +- If
         |                             +- Match
         |                             +- ImplicitMatch
         |                             +- Try
         |                             +- Return
         |                             +- Repeated
         |                             +- Inlined
         |                             +- SelectOuter
         |                             +- While
         |
         +- TypeTree ----+- Inferred
         |               +- TypeIdent
         |               +- TypeSelect
         |               +- Projection
         |               +- Singleton
         |               +- Refined
         |               +- Applied
         |               +- Annotated
         |               +- MatchTypeTree
         |               +- ByName
         |               +- LambdaTypeTree
         |               +- Bind
         |
         +- TypeBoundsTree
         +- SyntheticBounds
         +- CaseDef
         +- TypeCaseDef

+- Pattern --+- Value
             +- Bind
             +- Unapply
             +- Alternative
             +- TypeTest


                 +- NoPrefix
+- TypeOrBounds -+- TypeBounds
                 |
                 +- Type -------+- ConstantType
                                +- SymRef
                                +- TermRef
                                +- TypeRef
                                +- SuperType
                                +- Refinement
                                +- AppliedType
                                +- AnnotatedType
                                +- AndType
                                +- OrType
                                +- MatchType
                                +- ByNameType
                                +- ParamRef
                                +- ThisType
                                +- RecursiveThis
                                +- RecursiveType
                                +- LambdaType[ParamInfo <: TypeOrBounds] -+- MethodType
                                                                          +- PolyType
                                                                          +- TypeLambda

+- ImportSelector -+- SimpleSelector
                   +- RenameSelector
                   +- OmitSelector

+- Id

+- Signature

+- Position

+- Comment

+- Constant

+- Symbol --+- PackageDefSymbol
            +- ClassDefSymbol
            +- TypeDefSymbol
            +- TypeBindSymbol
            +- DefDefSymbol
            +- ValDefSymbol
            +- BindSymbol
            +- NoSymbol

```

## More Examples

* Start experimenting with TASTy Reflect ([link](https://github.com/nicolasstucki/tasty-reflection-exercise))

