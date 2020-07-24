package tests

/** # AN IMPORTANT TEST CLASS
  *
  * Very important, in fact
  * -----------------------
  *
  * ### So important it gets three headers
  *
  * This is an *important* _test_ class. And `this` is inline code.
  *
  * And this is the **strong** __emphasis__ test.
  *
  * And this
  * ```scala
  * is.an("actual code block")
  * ```
  *
  * And this
  *
  *      is.an("indented code block")
  *
  * And this
  * > is
  * > > a
  * > blockquote
  *
  *
  * @author Gal Anonim
  * @version 1.0.0
  * @result A class doesn't actually have a result.
  * @constructor A class has a constructor, and this one is important.
  */
class A

/** = An important Wiki test class =
  *
  * == Very important, in fact ==
  *
  * === So important it gets three headers ===
  *
  * This is an ''important'' '''test''' __class__. And `this` is inline code.
  *
  * While
  * {{{
  * this.is("a code block")
  * }}}
  *
  * @syntax wiki
  */
class B extends A
class C 
class D[T]
class E[T] extends D[T]



class Constructors(a: String):
    def this() = this("Ala")
    def this(a: A)(b: A) = this("Ala")

/** Some methods to tests */
class Methods:
 def nobraces: A = ???
 def simple(): B = ???
 def oneParam(a: A): B = ???
 def multipleParams(a: A, b: B): C = ???
 def vararg(a: A*): C = ???
 def multipleList(a: A)(b: B): C = ???

 def generic[T](a: D[T]): D[T] = ???
 def generic2[T, V](a: D[T], b: E[V]): D[T] = ???

 def primitives(a: Int, b: Double, c: Short): Byte = 0
 def strings(a: String): String = ""
 def arrays(a: Array[String], b: Array[Int]): Array[Double] = ???
