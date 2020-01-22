---
layout: doc-page
title: How to write a type class `derived` method using macros
---

In the main [derivation](./derivation.md) documentation page we explaind the
details behind `Mirror`s and type class derivation. Here we demonstrate how to
implement a type class `derived` method using macros only. We follow the same
example of deriving `Eq` instances and for simplicity we support a `Product`
type e.g., a case class `Person`. The low-level method we will use to implement
the `derived` method exploits quotes, splices of both expressions and types and
the `scala.quoted.matching.summonExpr` method which is the equivalent of
`summonFrom`. The former is suitable for use in a quote context, used within
macros.

As in the original code, the type class definition is the same:

```scala
trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}
```

we need to implement a method `Eq.derived` on the companion object of `Eq` that
produces an instance for `Eq[T]` given a `Mirror[T]`. Here is a possible
signature,

```scala
def derived[T: Type](ev: Expr[Mirror.Of[T]])(given qctx: QuoteContext): Expr[Eq[T]] = ???
```

and for comparison reasons we give the same signature with had with `inline`:

```scala
inline given derived[T]: (m: Mirror.Of[T]) => Eq[T] = ???
```

Note, that since a type is used in a subsequent stage it will need to be lifted
to a `Type` by using the corresponding context bound. The body of this method is
shown below:


```scala
def derived[T: Type](m: Expr[Mirror.Of[T]])(given qctx: QuoteContext): Expr[Eq[T]] = {
  import qctx.tasty.{_, given}

  val elementTypes = m match {
    case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = $elem } } => elem
  }

  val elemInstances = summonAll(elementTypes)

  val eqProductBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
    elemInstances.zipWithIndex.foldLeft(Expr(true: Boolean)) {
      case (acc, (elem, index)) =>
        val e1 = '{$x.asInstanceOf[Product].productElement(${Expr(index)})}
        val e2 = '{$y.asInstanceOf[Product].productElement(${Expr(index)})}
        '{ $acc && $elem.asInstanceOf[Eq[Any]].eqv($e1, $e2) }
    }
  }

  '{
    eqProduct((x: T, y: T) => ${eqProductBody('x, 'y)})
  }
}
```

Note, that in the `inline` case we can merely write
`summonAll[m.MirroredElemTypes]` inside the inline method but here, since
`summonExpr` is required if we need to query the context we need to extract the
element types in a macro fashion. Being inside a macro, our first reaction would
be to write the code below. Since the path inside the type argument is not
stable this cannot be used:

```scala
'{
  summonAll[$m.MirroredElemTypes]
}
```

Instead we extract the tuple-type for element types using pattern matching over
quotes and more specifically of the refined type:

```scala
 case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = $elem } } => elem
```

The implementation of `summonAll` as a macro can be show below:

```scala
def summonAll[T](t: Type[T])(given qctx: QuoteContext): List[Expr[Eq[_]]] = t match {
  case '[$tpe *: $tpes] => summonExpr(given '[Eq[$tpe]]).get :: summonAll(tpes)
  case '[Unit] => Nil
}
```

Note, that in a realistic implementation the `summonExpr(given '[Eq[$tpe]]).get`
is going to fail if the necessary given instances for some type are not present.

One additional difference with the body of `derived` here as opposed to the one
with `inline` is that with macros we need to synthesize the body of the code during the
macro-expansion time. That is the rationale behind the `eqProductBody` function.
Assuming that we calculate the equality of two `Person`s defined with a case
class that holds a name of type `String` and an age of type `Int`, the equality
check we want to generate is the following:

```scala
true
  && Eq[String].eqv(x.productElement(0),y.productElement(0))
  && Eq[Int].eqv(x.productElement(1), y.productElement(1))
```

### Calling the derived method inside the macro

Following the rules in [Macros](../metaprogramming.md) we create two methods.
One that hosts the top-level splice `eqv` and one that is the implementation.

```scala
inline def eqv[T](value: =>T, value2: =>T): Boolean = ${ eqvImpl('value, 'value2) }

def eqvImpl[T: Type](value: Expr[T], value2: Expr[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty.{_, given}

  val mirrorTpe = '[Mirror.Of[T]]
  val mirrorExpr = summonExpr(given mirrorTpe).get
  val derivedInstance = Eq.derived(mirrorExpr)

  '{
    $derivedInstance.eqv($value, $value2)
  }
}
```

Note, that we need to quote the type we need `Mirror.Of[T]` with the quoted
syntax for types and then trigger its synthesis with `summonExpr`. `mirrorExpr`
now holds the refined type for e.g., a `Person`:

```scala
scala.deriving.Mirror {
  type MirroredType >: Person <: Person
  type MirroredMonoType >: Person <: Person
  type MirroredElemTypes >: scala.Nothing <: scala.Tuple
} & scala.deriving.Mirror.Product {
  type MirroredMonoType >: Person <: Person
  type MirroredType >: Person <: Person
  type MirroredLabel >: "Person" <: "Person"
} {
  type MirroredElemTypes >: scala.*:[scala.Predef.String, scala.*:[scala.Int, scala.Unit]] <: scala.*:[scala.Predef.String, scala.*:[scala.Int, scala.Unit]]
  type MirroredElemLabels >: scala.*:["name", scala.*:["age", scala.Unit]] <: scala.*:["name", scala.*:["age", scala.Unit]]
}
```

The derived instance then is finally generated with:

```scala
 val derivedInstance = Eq.derived(mirrorExpr)

  '{
    $derivedInstance.eqv($value, $value2)
  }
```

The full code is shown below:

```scala
import scala.deriving._
import scala.quoted._
import scala.quoted.matching._

object Macro {

  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    given Eq[String] {
      def eqv(x: String, y: String) = x == y
    }

    given Eq[Int] {
      def eqv(x: Int, y: Int) = x == y
    }

    def eqProduct[T](body: (T, T) => Boolean): Eq[T] =
      new Eq[T] {
        def eqv(x: T, y: T): Boolean = body(x, y)
      }

    def summonAll[T](t: Type[T])(given qctx: QuoteContext): List[Expr[Eq[_]]] = t match {
      case '[$tpe *: $tpes] => summonExpr(given '[Eq[$tpe]]).get :: summonAll(tpes)
      case '[Unit] => Nil
    }

    def derived[T: Type](ev: Expr[Mirror.Of[T]])(given qctx: QuoteContext): Expr[Eq[T]] = {
      import qctx.tasty.{_, given}

      val elementTypes = ev match {
        case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = $elem } } => elem
      }

      val elemInstances = summonAll(elementTypes)

      val eqProductBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) => {
        elemInstances.zipWithIndex.foldLeft(Expr(true: Boolean)) {
          case (acc, (elem, index)) =>
            val e1 = '{$x.asInstanceOf[Product].productElement(${Expr(index)})}
            val e2 = '{$y.asInstanceOf[Product].productElement(${Expr(index)})}
            '{ $acc && $elem.asInstanceOf[Eq[Any]].eqv($e1, $e2) }
        }
      }

      '{
        eqProduct((x: T, y: T) => ${eqProductBody('x, 'y)})
      }
    }
  }

  inline def eqv[T](value: =>T, value2: =>T): Boolean = ${ eqvImpl('value, 'value2) }

  def eqvImpl[T: Type](value: Expr[T], value2: Expr[T])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty.{_, given}

    val mirrorTpe = '[Mirror.Of[T]]
    val mirrorExpr = summonExpr(given mirrorTpe).get
    val derivedInstance = Eq.derived(mirrorExpr)

    '{
      $derivedInstance.eqv($value, $value2)
    }
  }
}
```