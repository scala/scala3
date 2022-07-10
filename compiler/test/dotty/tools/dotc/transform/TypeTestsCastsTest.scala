package dotty.tools
package dotc
package transform

import core.*
import Contexts.*, Decorators.*, Denotations.*, SymDenotations.*, Symbols.*, Types.*
import Annotations.*

import org.junit.Test
import org.junit.Assert.*

class TypeTestsCastsTest extends DottyTest:
  val defn = ctx.definitions; import defn.*

  @Test def orL = checkFound(List(StringType, LongType), OrType(LongType, StringType, false))
  @Test def orR = checkFound(List(LongType, StringType), OrType(StringType, LongType, false))

  @Test def annot = checkFound(List(StringType, LongType), AnnotatedType(OrType(LongType, StringType, false), Annotation(defn.UncheckedAnnot)))

  @Test def andL = checkFound(List(StringType), AndType(StringType, AnyType))
  @Test def andR = checkFound(List(StringType), AndType(AnyType, StringType))
  @Test def andX = checkFound(List(NoType), AndType(StringType, BooleanType))

  // (A | B) & C       => {(A & B), (A & C)}
  // A & (B | C)       => {(A & B), (A & C)}
  // (A | B) & (C | D) => {(A & C), (A & D), (B & C), (B & D)}
  @Test def orInAndL = checkFound(List(StringType, LongType), AndType(OrType(LongType, StringType, false), AnyType))
  @Test def orInAndR = checkFound(List(StringType, LongType), AndType(AnyType, OrType(LongType, StringType, false)))
  @Test def orInAndZ =
    // (Throwable | Exception) & (RuntimeException | Any) =
    //   Throwable & RuntimeException = RuntimeException
    //   Throwable & Any              = Throwable
    //   Exception & RuntimeException = RuntimeException
    //   Exception & Any              = Exception
    val ExceptionType = defn.ExceptionClass.typeRef
    val RuntimeExceptionType = defn.RuntimeExceptionClass.typeRef
    val tp = AndType(OrType(ThrowableType, ExceptionType, false), OrType(RuntimeExceptionType, AnyType, false))
    val exp = List(ExceptionType, RuntimeExceptionType, ThrowableType, RuntimeExceptionType)
    checkFound(exp, tp)

  def checkFound(found: List[Type], tp: Type) =
    val expected = found.map(_.classSymbol)
    val obtained = TypeTestsCasts.foundClasses(tp)
    assertEquals(expected, obtained)
end TypeTestsCastsTest
