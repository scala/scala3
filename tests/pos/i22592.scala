import scala.quoted.*

trait Foo:
  def inherited = ()

object Bar extends Foo:
  def local = ()
  def localArg(arg: Any) = ()

  def macro1(using Quotes): Expr[Unit] = '{ local }
  def macro2(using Quotes): Expr[Unit] = '{ Bar.inherited }
  def macro3(using Quotes): Expr[Unit] = '{ inherited }
  def macro4(using Quotes): Expr[Unit] = '{ this.local }
  def macro5(using Quotes): Expr[Unit] = '{ this.inherited }
  def macro6(using Quotes): Expr[Unit] = '{ localArg(this) }
