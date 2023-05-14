package annot

import com.javacp.annot.*
import scala.annotation.meta.*
import scala.language.experimental.macros

@ClassAnnotation
class Annotations[@TypeParameterAnnotation T](@ParameterAnnotation x: T) { self: AnyRef =>
  @FieldAnnotation
  val field = 42

  @MethodAnnotation
  def method = {
    @LocalAnnotation
    val local = 42
    local
  }
  @TypeAnnotation
  type S
}

class B @ConstructorAnnotation()(x: Int) {
  @ConstructorAnnotation
  def this() = this(42)

  @throws[Exception]
  def throwing = throw new Exception("")
}

@ObjectAnnotation
object M {
  @MacroAnnotation
  def m[TT]: Int = macro ???
}

@TraitAnnotation
trait T

object Alias {
  type A = ClassAnnotation @param
}
