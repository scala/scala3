package tests
package snippetCompilerTests

/**
  * ```scala sc:fail
  * //{
  * import scala.collection.Seq
  * //}
  *
  * def a = 2
  * val x = 1 + List()
  * a
  *
  * try {
  *   2+3
  * }
  *
  * /*
  *   Huge comment
  * */
  * val xd: String = 42
  *
  * def a(i: Int): Boolean = i match // This is a function
  *   case 1 => true
  *
  * val b: Int = 2.3 /* Also quite a big comment */
  *
  * val d: Long = "asd"
  * ```
  *
  * ```scala sc:fail
  * def a = 2
  * val x = 1 + List()
  * a
  * ```
  *
  * ```scala sc:compile
  * def a = 2
  * ```
  *
  * ```scala sc:nocompile
  * def a = 3
  * a()
  * ```
  */
class A {
  trait B
}

/**
 * ```scala sc:fail
 * val c: Int = 4.5
 * ```
 */
class B { }

/**
 * ```scala sc:macrocompile
 *    inline def sum(args: Int*): Int = ${ sumExpr('args) }
 *    def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
 *      case Varargs(Exprs(args)) => ???
 *        // args: Seq[Int]
 * ```
 */
class C { }

/**
   * ```scala sc:usingquotes
   *    import quotes.reflect.*
   *    '{ List(${Varargs(List('{1}, '{2}, '{3}))}: _*) } // equivalent to '{ List(1, 2, 3) }
   * ```
   */
class D { }

trait Quotes {
  val reflect: reflectModule = ???
  trait reflectModule { self: reflect.type =>
    /**
      * ```scala sc:fail
      * 2 + List()
      * ```
      *
      */
    def a = 3
  }
}
