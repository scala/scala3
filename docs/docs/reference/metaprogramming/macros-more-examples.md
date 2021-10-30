---
title: "Macros: More Examples"
layout: singlepage-overview
scala3: true
---

> The goal for this page is to contain additional macro meta-programming examples.
> If you have a basic self-containing macro example, please add it by [submitting a PR](https://github.com/lampepfl/dotty/edit/master/docs/docs/reference/metaprogramming/macros-more-examples.md).  


## How to get the companion object of a generic class?

```scala
import scala.quoted.*
transparent inline def getCompanion[E]: AnyRef = ${ getCompanionMacro[E] }
def getCompanionMacro[E](using Quotes, Type[E]): Expr[AnyRef] =
  import quotes.reflect.*
  val companionSym = TypeRepr.of[E].typeSymbol.companionModule
  Ref(companionSym).asExprOf[AnyRef]
```

```scala
trait Foo
object Foo

enum MyEnum:
  case Baz, Bar

val foo : Foo.type = getCompanion[Foo]
val myEnum : MyEnum.type = getCompanion[MyEnum]
```

## How to convert a `TypeRepr` into a `TypeTree`
This example also teaches how to create an extension method for meta-programming.
```scala
import scala.quoted.*
extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def asTypeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    tpe.asType match
      case '[t] =>
        TypeTree.of[t]
```

## How to print-out an intermediate type in type-level programming
```scala
trait PrintType[T]
object PrintType:
  inline given [T]: PrintType[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[PrintType[T]] =
    import quotes.reflect.*
    println(TypeRepr.of[T].show)
    '{ new PrintType[T] {} }
```

```scala
import compiletime.ops.int.*
trait F1[T1 <: Int, T2 <: Int] {type Out = T1 * 5 + T2}
given F1 with {}
trait F2[T1 <: Int] {type Out = T1 * T1}
given F2 with {}

//composes `F2[F1[T1,T2]]`
trait F12[T1 <: Int, T2 <: Int] {type Out <: Int}
transparent inline given [T1 <: Int, T2 <: Int](using 
  f1 : F1[T1, T2]
)(using
  p : PrintType[f1.Out],
  f2 : F2[f1.Out]
) : F12[T1, T2] = new F12[T1, T2]:
  type Out = f2.Out

val f12 = summon[F12[1, 2]] //Prints-out `7`
summon[f12.Out =:= 49]
```

Minor note:
The composition example here is just to demonstrate how `PrintType` can be used. 
Alternatively, we could have defined `type F1[...] = T1 * 5 + T2` and `F2[...] = T1 * T1` and compose them simply with `F12[...] = F2[F1[T1, T2]]`.  