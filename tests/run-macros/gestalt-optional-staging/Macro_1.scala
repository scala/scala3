// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/Optional.scala
// using staging macros (only quotes and splices)

import scala.quoted._

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  inline def getOrElse[B >: A](alt: => B): B = ${ Optional.getOrElseImpl('this, 'alt) }

  inline def map[B >: Null](f: A => B): Optional[B] = ${ Optional.mapImpl('this, 'f) }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}

object Optional {

  // FIXME fix issue #5097 and enable private
  /*private*/ def getOrElseImpl[T >: Null : Staged](opt: Expr[Optional[T]], alt: Expr[T])(using QuoteContext): Expr[T] = '{
    if ($opt.isEmpty) $alt else $opt.value
  }

  // FIXME fix issue #5097 and enable private
  /*private*/ def mapImpl[A >: Null : Staged, B >: Null : Staged](opt: Expr[Optional[A]], f: Expr[A => B])(using QuoteContext): Expr[Optional[B]] = '{
    if ($opt.isEmpty) new Optional(null)
    else new Optional(${Expr.betaReduce(f)('{$opt.value})})
  }

}
