package oolong.phobos

import scala.quoted.*
import scala.compiletime.*
import scala.annotation.StaticAnnotation

final class xmlns[T](ns: T) extends StaticAnnotation
trait Namespace[T]{
    val getNamespace: String
}

object common{
  private def extractFeildNamespace(using Quotes)(
      fieldAnnotations: List[Expr[Any]],
  ): Expr[Option[String]] = {
    import quotes.reflect.*

    fieldAnnotations.collect { case '{ xmlns($namespace: b) } =>
      '{ Some(summonInline[Namespace[b]].getNamespace) }
    }
    ???
  }
}
