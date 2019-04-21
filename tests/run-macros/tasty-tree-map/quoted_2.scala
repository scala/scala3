
import Macros._

class Annot extends scala.annotation.Annotation

abstract class Foo {
  type T
  def y: T
}

object Test {

  def main(args: Array[String]): Unit = {
    println(identityMaped(32))
    println(identityMaped(32 + 1))
    println(identityMaped({ val i = 34; i }))
    println(identityMaped({ var i = 34; i += 1; i }))
    println(identityMaped({ var i = 0; while (i < 36) i += 1; i }))
    println(identityMaped({ var i = 0; do i += 1 while (i < 37); i }))
    println(identityMaped(try 38 finally ()))
    println(identityMaped(try 39 catch { case _: Error => }))
    println(identityMaped(new java.lang.Integer(40)))
    println(identityMaped(if (true: Boolean) 41 else -1))
    println(identityMaped(true match { case _ => 42 } ))
    println(identityMaped({ def f = 43; f }))
    println(identityMaped({ def f() = 44; f() }))
    println(identityMaped({ def f[T] = 45; f[Int] }))
    println(identityMaped({ def f: Int = return 46; f }))
    println(identityMaped({ def f(a: Int): Int = a; f(a = 47) }))
    println(identityMaped({ def f(a: Int*): Int = a.sum; f(47, 1) }))
    println(identityMaped(((a: Int) => a)(49)))
    println(identityMaped({ type A = Int; 50: A }))
    println(identityMaped({ import scala.{Int => I}; 51: I }))
    println(identityMaped(52 match { case x => x }))
    println(identityMaped(53 match { case x: Int => x }))
    println(identityMaped((0: Any) match { case _: Int | _: Double => 54 }))
    println(identityMaped(0 match { case I55(x) => x }))
    println(identityMaped({ val x: scala.Int = 56; val y: x.type = x; y }))
    println(identityMaped({ val x: scala.Int @Annot = 57; x }))
    println(identityMaped({ val x: List[Int] = 58 :: Nil; x.head }))
    println(identityMaped({ val x: Int & Any = 59; x }))
    println(identityMaped({ val x: Int | Any = 60; x }))
    println(identityMaped({ def f(x: => Int): Int = x; f(61) }))
    println(identityMaped({ type T[X] = X; val x: T[Int] = 62; x }))
    println(identityMaped({ type T[X] = X match { case Int => String; case String => Int }; val x: T[String] = 63; x }))
    println(identityMaped((Nil: List[Int]) match { case _: List[t] => 64 }))
    println(identityMaped({ object F { type T = Int }; val x: F.T = 65; x }))
    println(identityMaped({ val x: Foo { type T = Int } = new Foo { type T = Int; def y: Int = 66 }; x.y }))
  }

  object I55 {
    def unapply(arg: Any): Some[Int] = Some(55)
  }
}
