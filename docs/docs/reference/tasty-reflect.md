---
layout: doc-page
title: "TASTy reflect"
---

TASTy reflect provides an API that allows inspection and construction of Typed Abstract Syntax Trees (TAST). 
It may be used on quoted expressions (`quoted.Expr`) and types (`quoted.Type`) from [Principled Meta-programming](./principled-meta-programming.html) 
or on full TASTy files.

If you are starting using macros see first [Principled Meta-programming](./principled-meta-programming.html) and then follow with API (if really needed).


## From quotes and splices to TASTs and back

`quoted.Expr` and `quoted.Type` are opaque TASTs. 
The opaqueness required in [Principled Meta-programming](./principled-meta-programming.html) provide the guarantee that 
the generation of code of the macro will be type correct.
Using TASTy reflect will break these guarantees and may fail at macro expansion time, hence additional explicit check must be done. 


To provide reflection capabilities in macro we need to add an implicit parameter of type `scala.tasty.Reflection` and import it in the scope where it is used.

```scala
import scala.quoted._
import scala.tasty._

inline def natConst(x: Int): Int = ~natConstImpl('(x))

def natConstImpl(x: Expr[Int])(implicit reflection: Reflection): Expr[Int] = {
  import reflection._
  ...
}
```

`import reflection._` will provide a `reflect` extension method on `quoted.Expr` and `quoted.Type` with return a `reflection.Term` and `reflection.TypeTree` respectivly.
It will also import all extractors and methods on TASTy reflect trees. For example the `Term.Literal(_)` extractor used bellow.
To easily know which extractor are needed the `reflection.Term.show` method returns the string representation of the extractors.


```scala
def natConstImpl(x: Expr[Int])(implicit reflection: Reflection): Expr[Int] = {
  import reflection._
  val xTree: Term = x.reflect
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

The method `reflection.Term.reify[T]` provides a way to to go back to a `quoted.Expr`.
Note that the type must be set explicitly and that if it does not conform to it an exception will be thrown. 
In the code above we could have replaced `n.toExpr` by `xTree.reify[Int]`.
 
## ASTs of a TASTy file

To inspect the TASTy trees of a TASTy file a consumer can be defined in the following way. 

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
ConsumeTasty(classpath, List("foo.Bar"), new Consumer)
```
 
## TASTy reflect ASTs

TASTy reflect provides the following types as defined in `scala.tasty.reflect.Core`.

```none
+- Tree -+- PackageClause
         +- Import
         +- Statement -+- Definition --+- PackageDef
                       |               +- ClassDef
                       |               +- TypeDef
                       |               +- DefDef
                       |               +- ValDef
                       |
                       +- Term --------+- Ident
                                       +- Select
                                       +- Literal
                                       +- This
                                       +- New
                                       +- NamedArg
                                       +- Apply
                                       +- TypeApply
                                       +- Super
                                       +- Typed
                                       +- Assign
                                       +- Block
                                       +- Lambda
                                       +- If
                                       +- Match
                                       +- Try
                                       +- Return
                                       +- Repeated
                                       +- Inlined
                                       +- SelectOuter
                                       +- While
                                       +- DoWhile


                       +- TypeTree ----+- Synthetic
                       |               +- Ident
                       |               +- Select
                       |               +- Project
                       |               +- Singleton
+- TypeOrBoundsTree ---+               +- Refined
                       |               +- Applied
                       |               +- Annotated
                       |               +- And
                       |               +- Or
                       |               +- MatchType
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

+- Constant

+- Symbol --+- PackageSymbol
            +- ClassSymbol
            +- TypeSymbol
            +- DefSymbol
            +- ValSymbol
            +- BindSymbol
            +- NoSymbol

Aliases:
 # TermOrTypeTree = Term | TypeTree
```

## Other resources 

* Start plaing TASTy reflect ([link](https://github.com/nicolasstucki/tasty-reflection-exercise))

