package annot

import com.javacp.annot._
import scala.annotation.meta._
import scala.language/*=>>scalaShadowing.language.*/.experimental/*=>>scalaShadowing.language.experimental.*/.macros/*=>>scalaShadowing.language.experimental.macros.*/

@ClassAnnotation
class /*=>>java.lang.Object#`<init>`().*/Annotations/*<<=annot.Annotations#*/[@Type/*<<=annot.Annotations#`<init>`().*/ParameterAnnotation T/*<<=annot.Annotations#`<init>`().(T)*//*<<=annot.Annotations#(T)*/](@ParameterAnnotation x/*<<=annot.Annotations#`<init>`().(x)*//*<<=annot.Annotations#(x)*/: T/*=>>annot.Annotations#`<init>`().(T)*/) { self/*<<=local0*/: AnyRef/*=>>scala.AnyRef#*/ =>
  @FieldAnnotation
  val field/*<<=annot.Annotations#field.*/ = 42

  @MethodAnnotation
  def method/*<<=annot.Annotations#method().*/ = {
    @LocalAnnotation
    val local/*<<=local1*/ = 42
    local/*=>>local1*/
  }
  @TypeAnnotation
  type S/*<<=annot.Annotations#S#*/
}

class /*=>>java.lang.Object#`<init>`().*/B/*<<=annot.B#*/ @Cons/*<<=annot.B#`<init>`().*/tructorAnnotation()(x/*<<=annot.B#`<init>`().(x)*//*<<=annot.B#(x)*/: Int/*=>>scala.Int#*/) {
  @ConstructorAnnotation
  def this()/*<<=annot.B#`<init>`(+1).*/ = this(/*=>>annot.B#`<init>`().*/42)
}

@ObjectAnnotation
object /*=>>java.lang.Object#`<init>`().*/M/*<<=annot.M.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>annot.M.*/@MacroAnnotation
  def m/*<<=annot.M.m().*/[TT/*<<=annot.M.m().(TT)*/]: Int/*=>>scala.Int#*//*=>>scala.Predef.`???`().*//*=>>scala.Predef.*//*=>>scala.*//*=>>_root_*/ = macro ???
}

@TraitAnnotation
trait T/*<<=annot.T#*/

object /*=>>java.lang.Object#`<init>`().*/Alias/*<<=annot.Alias.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>annot.Alias.*/type A/*<<=annot.Alias.A#*/ = ClassAnnotation/*=>>com.javacp.annot.ClassAnnotation#*/ @param/*=>>scala.annotation.meta.param#*//*=>>scala.annotation.meta.param#`<init>`().*/
}
