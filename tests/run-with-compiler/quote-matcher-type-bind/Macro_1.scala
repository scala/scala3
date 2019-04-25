import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.internal.quoted.Matcher._
import scala.internal.Quoted._

object Macros {

  inline def swapFandG(x: => Unit): Unit = ${impl('x)}

  private def impl(x: Expr[Unit])(implicit reflect: Reflection): Expr[Unit] = {

    type TT // type binding
    object DSLf {
      def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tuple2[Type[_], Expr[Any]]] =
        scala.internal.quoted.Matcher.unapply[Tuple2[Type[_], Expr[Any]]](x)('{type t; DSL.f[Hole[t]](patternHole[t]) }, reflect)
    } // case '{ DSL.f[$t]($x) } =>

    x match {
      // case '{ DSL.f[$t]($x) } =>
      // case scala.internal.quoted.Matcher.unapply[Tuple2[Type[tt @ _], Expr[tt]]](Tuple2(t, TypedExpr(x)(`t`, reflect))(/*implicits*/ '{ DSL.f[Hole[Nothing, Any]](hole[Any])] }, reflect) =>
      case DSLf((t: Type[tt], x: Expr[t2])) =>

        implicit val tt = t
        '{ DSL.g[$t]($x) }

      // case '{ DSL.f[$t]($x) } =>
      //      case DSLg(t, x) =>
      //        '{ DSL.f[$t]($x) }
???
      case _ =>
        x
    }
  }

}


object TypedExpr {
  def unapply[T](arg: Expr[_])(implicit t: Type[T], reflect: Reflection): Option[Expr[T]] = {
    import reflect._
    if (arg.unseal.tpe <:< t.unseal.tpe) Some(arg.asInstanceOf[Expr[T]])
    else None
  }
}


//
// DSL in which the user write the code
//

object DSL {
  def f[T](x: T): Unit = println("f: " + x.toString)
  def g[T](x: T): Unit = println("g: " + x.toString)
}

//
// Helper to abstract call to scala.internal.quoted.Matcher.unapply and setup an object with the unapply
//

class ExprMatch[Tup <: Tuple](pattern: Expr[_]) {
  def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tup] =
    scala.internal.quoted.Matcher.unapply[Tup](x)(pattern, reflect)
}


//class a {
//
//  def foo(x: Any) =
//
//    new Foo { type T = Int; type Tup = (Int, List[Int]); val contents: Tup = (3, 5 :: Nil) } match {
//      case Bar(x, ls) =>
//        val l2 = x :: ls
//        l2
//    }
//  new Foo { val contents = ??? } match {
//    case Bar(x, ls) =>
//      val l2 = x :: ls
//      l2
//  }
//}
//
//
//
//trait Foo {
//  type T
//  type Tup <: Tuple
//  val contents: Tup
//}
//
//object Foo {
//  def unaplly[F <: Foo](arg: F): Option[arg.Tup] = Some(arg.contents)
//}
//
//object Bar {
//  def unapply(arg: Foo { type Tup = (T, List[T]) }): Option[arg.Tup] =
//    Foo.unaplly[Foo { type Tup = (T, List[T]) }](arg)
//}