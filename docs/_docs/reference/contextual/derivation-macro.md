---
layout: doc-page
title: "How to write a type class `derived` method using macros"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/derivation-macro.html
---

In the main [derivation](./derivation.md) documentation page, we explained the details behind `Mirror`s and type class derivation.
Here we demonstrate how to implement a type class `derived` method using macros only.
We follow the same example of deriving `Eq` instances and for simplicity we support a `Product` type e.g., a case class `Person`.
The low-level technique that we will use to implement the `derived` method exploits quotes, splices of both expressions and types and the `scala.quoted.Expr.summon` method which is the equivalent of `scala.compiletime.summonFrom`.
The former is suitable for use in a quote context, used within macros.

As in the original code, the type class definition is the same:

```scala
trait Eq[T]:
  def eqv(x: T, y: T): Boolean
```

We need to implement an inline method `Eq.derived` on the companion object of `Eq` that calls into a macro to produce a quoted instance for `Eq[T]`.
Here is a possible signature:


```scala
inline def derived[T]: Eq[T] = ${ derivedMacro[T] }

def derivedMacro[T: Type](using Quotes): Expr[Eq[T]] = ???
```

Note, that since a type is used in a subsequent macro compilation stage it will need to be lifted to a `quoted.Type` by using the corresponding context bound (seen in `derivedMacro`).


For comparison, here is the signature of the inline `derived` method from the [main derivation page](./derivation.md):
```scala
inline def derived[T](using m: Mirror.Of[T]): Eq[T] = ???
```

Note that the macro-based `derived` signature does not have a `Mirror` parameter.
This is because we can summon the `Mirror` inside the body of `derivedMacro` thus we can omit it from the signature.

One additional possibility with the body of `derivedMacro` here as opposed to the one with `inline` is that with macros it is simpler to create a fully optimised method body for `eqv`.

Let's say we wanted to derive an `Eq` instance for the following case class `Person`,
```scala
case class Person(name: String, age: Int) derives Eq
```

the equality check we are going to generate is the following:

```scala
(x: Person, y: Person) =>
  summon[Eq[String]].eqv(x.productElement(0), y.productElement(0))
  && summon[Eq[Int]].eqv(x.productElement(1), y.productElement(1))
```

> Note that it is possible, by using the [reflection API](../metaprogramming/reflection.md), to further optimise and directly reference the fields of `Person`, but for clear understanding we will only use quoted expressions.

The code to generates this body can be seen in the `eqProductBody` method, shown here as part of the definition for the `derivedMacro` method:


```scala
def derivedMacro[T: Type](using Quotes): Expr[Eq[T]] =

  val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

  ev match
    case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} =>
      val elemInstances = summonInstances[T, elementTypes]
      def eqProductBody(x: Expr[Product], y: Expr[Product])(using Quotes): Expr[Boolean] = {
        if elemInstances.isEmpty then
          Expr(true)
        else
          elemInstances.zipWithIndex.map {
            case ('{ $elem: Eq[t] }, index) =>
              val indexExpr = Expr(index)
              val e1 = '{ $x.productElement($indexExpr).asInstanceOf[t] }
              val e2 = '{ $y.productElement($indexExpr).asInstanceOf[t] }
              '{ $elem.eqv($e1, $e2) }
          }.reduce((acc, elem) => '{ $acc && $elem })
        end if
      }
      '{ eqProduct((x: T, y: T) => ${eqProductBody('x.asExprOf[Product], 'y.asExprOf[Product])}) }

    // case for Mirror.SumOf[T] ...
```

Note, that in the version without macros, we can merely write `summonInstances[T, m.MirroredElemTypes]` inside the inline method but here, since `Expr.summon` is required, we can extract the element types in a macro fashion.
Being inside a macro, our first reaction would be to write the code below:

```scala
'{
  summonInstances[T, $m.MirroredElemTypes]
}
```

However, since the path inside the type argument is not stable this cannot be used.
Instead we extract the tuple-type for element types using pattern matching over quotes and more specifically of the refined type:

```scala
   case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} => ...
```

Shown below is the implementation of `summonInstances` as a macro, which for each type `elem` in the tuple type, calls
`deriveOrSummon[T, elem]`.

To understand `deriveOrSummon`, consider that if `elem` derives from the parent `T` type, then it is a recursive derivation.
Recursive derivation usually happens for types such as `scala.collection.immutable.::`. If `elem` does not derive from `T`, then there must exist a contextual `Eq[elem]` instance.

```scala
def summonInstances[T: Type, Elems: Type](using Quotes): List[Expr[Eq[?]]] =
  Type.of[Elems] match
    case '[elem *: elems] => deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case '[EmptyTuple]    => Nil

def deriveOrSummon[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
  Type.of[Elem] match
    case '[T] => deriveRec[T, Elem]
    case _    => '{ summonInline[Eq[Elem]] }

def deriveRec[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
  Type.of[T] match
    case '[Elem] => '{ error("infinite recursive derivation") }
    case _       => derivedMacro[Elem] // recursive derivation
```

The full code is shown below:

```scala
import compiletime.*
import scala.deriving.*
import scala.quoted.*


trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[String]:
    def eqv(x: String, y: String) = x == y

  given Eq[Int]:
    def eqv(x: Int, y: Int) = x == y

  def eqProduct[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = body(x, y)

  def eqSum[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = body(x, y)

  def summonInstances[T: Type, Elems: Type](using Quotes): List[Expr[Eq[?]]] =
    Type.of[Elems] match
      case '[elem *: elems] => deriveOrSummon[T, elem] :: summonInstances[T, elems]
      case '[EmptyTuple]    => Nil

  def deriveOrSummon[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
    Type.of[Elem] match
      case '[T] => deriveRec[T, Elem]
      case _    => '{ summonInline[Eq[Elem]] }

  def deriveRec[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
    Type.of[T] match
      case '[Elem] => '{ error("infinite recursive derivation") }
      case _       => derivedMacro[Elem] // recursive derivation

  inline def derived[T]: Eq[T] = ${ derivedMacro[T] }

  def derivedMacro[T: Type](using Quotes): Expr[Eq[T]] =

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    ev match
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonInstances[T, elementTypes]
        def eqProductBody(x: Expr[Product], y: Expr[Product])(using Quotes): Expr[Boolean] = {
          if elemInstances.isEmpty then
            Expr(true)
          else
            elemInstances.zipWithIndex.map {
              case ('{ $elem: Eq[t] }, index) =>
                val indexExpr = Expr(index)
                val e1 = '{ $x.productElement($indexExpr).asInstanceOf[t] }
                val e2 = '{ $y.productElement($indexExpr).asInstanceOf[t] }
                '{ $elem.eqv($e1, $e2) }
            }.reduce((acc, elem) => '{ $acc && $elem })
          end if
        }
        '{ eqProduct((x: T, y: T) => ${eqProductBody('x.asExprOf[Product], 'y.asExprOf[Product])}) }

      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonInstances[T, elementTypes]
        val elements = Expr.ofList(elemInstances)

        def eqSumBody(x: Expr[T], y: Expr[T])(using Quotes): Expr[Boolean] =
          val ordx = '{ $m.ordinal($x) }
          val ordy = '{ $m.ordinal($y) }
          '{ $ordx == $ordy && $elements($ordx).asInstanceOf[Eq[Any]].eqv($x, $y) }

        '{ eqSum((x: T, y: T) => ${eqSumBody('x, 'y)}) }
  end derivedMacro
end Eq
```
