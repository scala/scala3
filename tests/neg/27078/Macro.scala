package sample

import scala.quoted.*
import sample.JavaAnnotation

object Macro:
  inline def valueFromAnnotation[T]: String =
    ${ valueFromAnnotationMacro[T] }

  def valueFromAnnotationMacro[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*

    val symbol = TypeRepr
      .of[A]
      .typeSymbol
    
    symbol.getAnnotation(TypeRepr.of[JavaAnnotation].typeSymbol) match
//    symbol.getAnnotation(TypeRepr.of[AnnotWithValue].typeSymbol) match
      case Some(value) =>
        '{ ${ value.asExprOf[JavaAnnotation] }.defaultedValue() }
//        '{ ${ value.asExprOf[AnnotWithValue] }.value }

      case None => report.errorAndAbort(s"No such annotation for ${symbol.fullName}")
    
  class AnnotWithValue(val value: String) extends scala.annotation.Annotation
