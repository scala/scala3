import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ~assertImpl('(condition), '(""))

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    split(cond) match {
      case app0: SealedApplication0[Boolean] =>
       '{
          val pre: ~app0.prefix.tpe = ~app0.prefix.expr
          scala.Predef.assert(~app0.fun('(pre)))
        }
      case app1: SealedApplication1[Boolean] =>
       '{
          val pre: ~app1.prefix.tpe = ~app1.prefix.expr
          val arg: ~app1.arg.tpe = ~app1.arg.expr
          scala.Predef.assert(~app1.fun('(pre))('(arg)))
        }
      case _: NotApplication[Boolean] =>
        '{ scala.Predef.assert(~cond) }
    }
  }

  def split[T: Type](arg: Expr[T])(implicit refl: Reflection): SealedApplication[T] = {
    import refl._
    arg.unseal.underlyingArgument match {
      case Term.IsSelect(sel @ Term.Select(prefix, op)) =>
        val sealedPrefix = prefix.seal
        val application = (prefix: Expr[sealedPrefix.Tpe]) => Term.Select.copy(sel)(prefix.unseal, op).seal.asExprOf[T]
        new SealedApplication0(sealedPrefix, application)
      case Term.Apply(Term.IsSelect(sel @ Term.Select(prefix, op)), arg1 :: Nil) =>
        val sealedPrefix = prefix.seal
        val sealedArg1 = arg1.seal
        val application = (prefix: Expr[sealedPrefix.Tpe]) => (arg: Expr[sealedArg1.Tpe]) => Term.Apply(Term.Select.copy(sel)(prefix.unseal, op), arg.unseal :: Nil).seal.asExprOf[T]
        new SealedApplication1(sealedPrefix, sealedArg1, application)
      case _ =>
        new NotApplication[T]
    }
  }


  trait SealedApplication[T]
  class NotApplication[T] extends SealedApplication[T]
  class SealedApplication0[T](val prefix: Sealed, val fun: Expr[prefix.Tpe] => Expr[T]) extends SealedApplication[T]
  class SealedApplication1[T](val prefix: Sealed, val arg: Sealed, val fun: Expr[prefix.Tpe] => Expr[arg.Tpe] => Expr[T]) extends SealedApplication[T]


}