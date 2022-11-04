package tests
package typesSignatures

class A
{
  type A = Int
  type B[+T] = Seq[T]
  type C[A, B <: A] = Seq[B]
}

trait V
{
  type Ala[+J] <: Int
  type Ola[+T]
  type X
}

class Generic[T]

class Base
{
  type A
  type B = Int

  // Tests not support multiline signatures
  type MatchT[T] = T match { case String => Char case Int => Byte }

  // Tests do not support multiline signatures
  type Elem[X] = X match { case String => Char case Array[t] => t case Iterable[t] => t }

  type F = [X] => (x: X) => List[X]

  type G = Int => Int

  type H = () => String

  type I = (Int, String, Int) => (String, Int)

  type J = (a: A) => a.type

  type K = [A] => (a: A) => a.type
}

class Operators
{
  type Binary = String =:= Int

  // Infix annotation is not well supported in Dotty
  // import scala.annotation.infix
  // infix type op[A, B] = Int
  // type Binary2 = String op Int

  import scala.compiletime.ops.boolean.*
  type Unary = ![true]
}

trait ThisTypeTest
{
  def foo: this.type //expected: def foo: ThisTypeTest.this.type
}
