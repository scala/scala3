package test

import annotation.{tailrec, switch}
import collection.mutable.*

object typers {

  val names = List("a", "b", "c")
  val ints = List(1, 2, 3)

  object Inference {

    for ((name, n) <- names.lazyZip(ints))
      println(name.length + n)

    def double(x: Char): String = s"$x$x"

    "abc" flatMap double

  }
  object Eta {

    def fun(x: Int): Int = x + 1
    val foo = fun(_)
  }

  case class DefaultParams(init: String => String = identity)
  object DefaultParams {
    def foo(x: String => String = identity) = x("abc")

    foo()
  }

  def len[U](xs: List[U]): Int = xs match {
    case x :: xs1 => 1 + len(xs1)
    case Nil => 0
  }

  object returns {

    def foo(x: Int): Int = {
      return 3
    }
  }

  object tries {

    val x = try {
      "abc"
    } catch {
      case ex: java.io.IOException =>
        123
    } finally {
      println("done")
    }

    val y = try 2 catch Predef.identity

    val z = try 3 finally "abc"

    println("abc".toString)

  }

  class C {

    @tailrec final def factorial(acc: Int, n: Int): Int = n match {
      case 0 => acc
      case _ => factorial(acc * n, n - 1)
    }

    println(factorial(1, 10))


  }

  class Refinements {
    trait C { type T; def process(x: T): Int }
    val y: C { type T; val key: T; def process(x: T): Int } = ???
  }

  object Accessibility {

    class A {
      val x: String = "abc"
    }

    class B extends A {
      private val x: Int = 1
    }

    val b: B = new B
    val y = b.x
    val z: String = y

  }

  object Self {

    class A(self1: Int) { self =>

      def self1(x: Int) = x

      class B {
        val b = self
        val c: A = b
      }

      val a = self
      val c: A = a
    }


  }

  object Arrays {

    val arr = List("a", "b", "c").toArray
    val i = 2
    arr(i).charAt(0)

    val x = new ArrayBuffer[String] // testing overloaded polymorphic constructors

    val entries = Array("abc", "def")

    for ((x, i) <- entries.zipWithIndex)
      println(x)
  }

  object SeqExtractors {
    val y = names match {
      case List(x, z) => x
      case List(x) => x
      case List() => ""
    }
    val yy: String = y
  }


}
