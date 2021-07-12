package tests

/** # AN IMPORTANT TEST CLASS
  *
  * Very important, in fact
  * -----------------------
  *
  * ### So important it gets three headers
  *
  * This is an *important* _test_ class.
  * Important enough to get multiple sentences in its summary.
  *
  * Here is foo: $foo
  *
  * And `this` is inline code.
  *
  * And this is the **strong** __emphasis__ test.
  *
  * And this
  * ```scala
  * is.an("actual code block")
  * with.multiple("lines")
  * """
  * {{{
  * and an embedded Wiki code block for good measure
  * }}}
  * """
  * ```
  *
  * While
  * {{{
  * this.is("a Wiki code block")
  * """
  * ```
  * with an embedded Md code block for good measure
  * ```
  * """
  * }}}
  *
  * And this
  *
  *      is.an("indented code block")
  *      with.multiple("lines")
  *
  * And this
  * > is
  * > > a
  * > blockquote
  *
  * And this is a link: [[method]].
  *
  * This is another way to spell the same link: [[#method]].
  *
  * And this is another link: [[AA]].
  *
  * And this is another link: [[AA$]].
  *
  * And this is yet another link: [[tests.Methods]].
  *
  * Yet another: [[tests.Methods.simple]].
  *
  * And yet another: [[example.level2.Documentation]].
  *
  * This is my friend: [[tests.B]].
  *
  * And this is his companion: [[tests.B$ link to the companion]].
  *
  * And this is a link that failed to resolve [[absent]].
  *
  * And this is a link that failed to parse [[#]].
  *
  * @author Gal Anonim
  * @version 1.0.0
  * @result A class doesn't actually have a result.
  * @constructor A class has a constructor, and this one is important.
  *
  * @define foo Foo expanded.
  */
class A {

  type X = tests.B
  type Y <: tests.B

  /** This is my method.
    *
    * This is a link: [[AA]].
    *
    * This is another link: [[AA$]].
    *
    * And yet another: [[B]].
    */
  final def myMethod(s: String): String = s

  /** This is foo: $foo */
  def method(s: String): String = s

  class AA

  object AA
}

/** Companion object to test linking */
object A {
  /** Apparently this should work: [[A.bar]]. */
  def foo() = 0
  def bar() = 0
}

/** = An important Wiki test class =
  *
  * == Very important, in fact ==
  *
  * === So important it gets three headers ===
  *
  * This is an ''important'' '''test''' __class__. And `this` is inline code.
  *
  * Here is foo: $foo
  *
  * While
  * {{{
  * this.is("a code block")
  * }}}
  *
  * And this is a link: [[otherMethod]].
  *
  * And this is another link: [[BB]].
  *
  * And this is yet another link: [[tests.Methods]].
  *
  * Yet another: [[tests.Methods.simple]].
  *
  * And yet another: [[example.level2.Documentation]].
  *
  * This is my friend: [[tests.A]].
  *
  * And this is his companion: [[tests.A$]].
  * @syntax wiki
  * @define foo Bar, actually.
  */
class B extends A {
  /** @inheritdoc */ override def method(s: String): String = s

  /** This is my foo: $foo */
  def otherMethod(s: String): String = s

  class BB
}

class BModule {
  def foo = "foo"
}

/** Companion object to test linking.
  *
  * This is my member: [[B$.Z]]
  *
  * And this is my term member: [[B$.Z$]]
  *
  * This is my member, addressed differently: [[this.Z]]
  *
  * And this is my term member, addressed differently: [[this.Z$]]
  */
object B extends BModule {
  type Z = Int
  val Z: Int = 0
}

/** This is foo: $foo */
class C extends A {
  object CC
  class CC
}
class D[T]
class E[T] extends D[T]

package inner {
  object A
  class B {
    /** This resolves: [[A]]. */
    def foo() = ()
  }
}

val bar = "bar"

/** A class with a semi-non-trivial constructor.
  *
  * @param a Hello!
  */
class Constructors(a: String):
    def this() = this("Ala")
    def this(a: A)(b: A) = this("Ala")

/** Some methods to tests */
class Methods:
 def nobraces: A = ???
 /** Class doc test.
   */
 def simple(): B = ???
 def oneParam(a: A): B = ???
 def multipleParams(a: A, b: B): C = ???
 def vararg(a: A*): C = ???
 def multipleList(a: A)(b: B): C = ???
 def generic[T](a: D[T]): D[T] = ???

 /** A generic method.
   *
   * @author Gal Anonim
   * @author Gol Anonim
   * @version 1.0.0
   * @since 0.1.0
   * @todo Consider cleaning up this documentation
   * @todo Add more documentation
   * @note This method is more important than it looks.
   * @note Much more important.
   * @param a A param!
   * @param b Another param.
   * @tparam T A type param!
   * @tparam V
   * Another type param.
   * ```
   * with.a("near-pathological").description
   * ```
   * But, surprisingly, still displayed quite well.
   *
   * Even though this line should be separated from previous one.
   *
   * @throws java.lang.Error Throws errors.
   * @example
   * ```
   * (m : Methods).generic2(d(), e()): B
   * ```
   * @example
   * ```
   * (m : Methods).generic2(d2(), e2()): B
   * ```
   * @return Nothing at all!
   */
 def generic2[T, V](a: D[T], b: E[V]): D[T] = ???

 def primitives(a: Int, b: Double, c: Short): Byte = 0
 def strings(a: String): String = ""
 def arrays(a: Array[String], b: Array[Int]): Array[Double] = ???

/** @define foo O's foo.
  */
object O:

  /** This is foo: $foo */
  def method(s: String) = s
