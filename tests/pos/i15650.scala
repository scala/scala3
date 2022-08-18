class Rational

import scala.quoted.*

class TC

object meta:
  object rationalTE:
    def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Option[Rational] = ???

  object rationalTC:
    def unapply(using Quotes)(using TC)(tr: quotes.reflect.TypeRepr): Option[Rational] = ???

  def foo(using Quotes)(p: quotes.reflect.TypeRepr): Unit =
    val rationalTE(e) = p  // warn: pattern binding uses refutable extractor `meta.rationalTE`

  def bar(using Quotes)(using TC)(p: quotes.reflect.TypeRepr): Unit =
    val rationalTC(c) = p  // warn: pattern binding uses refutable extractor `meta.rationalTC`
