import scala.quoted.*

trait Foo:
  def inherited = ()

class Bar extends Foo:
  def local = ()
  def localArg(arg: Any) = ()

  def macro1(using Quotes): Expr[Unit] = '{ local } // error
  def macro3(using Quotes): Expr[Unit] = '{ inherited } // error
  def macro4(using Quotes): Expr[Unit] = '{ this.local } // error
  def macro5(using Quotes): Expr[Unit] = '{ this.inherited } // error
  def macro6(using Quotes): Expr[Unit] = '{ localArg(this) } // error // error
