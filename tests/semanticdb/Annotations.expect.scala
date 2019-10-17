package annot

import com.javacp.annot._
import scala.annotation.meta._
import scala.language/*=>>scalaShadowing.language.*/.experimental/*=>>scalaShadowing.language.experimental.*/.macros/*=>>scalaShadowing.language.experimental.macros.*/

@ClassAnnotation/*=>>com.javacp.annot.ClassAnnotation#*/
class /*=>>java.lang.Object#`<init>`().*/Annotations/*<<=annot.Annotations#*/[@Type/*<<=annot.Annotations#`<init>`().*/ParameterAnnotation/*=>>com.javacp.annot.TypeParameterAnnotation#*/ T/*<<=annot.Annotations#`<init>`().(T)*//*<<=annot.Annotations#(T)*/](@ParameterAnnotation/*=>>com.javacp.annot.ParameterAnnotation#*/ x/*<<=annot.Annotations#`<init>`().(x)*//*<<=annot.Annotations#(x)*/: T/*=>>annot.Annotations#`<init>`().(T)*/) { self/*<<=local0*/: AnyRef/*=>>scala.AnyRef#*/ =>
  @FieldAnnotation/*=>>com.javacp.annot.FieldAnnotation#*/
  val field/*<<=annot.Annotations#field.*/ = 42

  @MethodAnnotation/*=>>com.javacp.annot.MethodAnnotation#*/
  def method/*<<=annot.Annotations#method().*/ = {
    @LocalAnnotation/*=>>com.javacp.annot.LocalAnnotation#*/
    val local/*<<=local1*/ = 42
    local/*=>>local1*/
  }
  @TypeAnnotation/*=>>com.javacp.annot.TypeAnnotation#*/
  type S/*<<=annot.Annotations#S#*/
}

class /*=>>java.lang.Object#`<init>`().*/B/*<<=annot.B#*/ @Cons/*<<=annot.B#`<init>`().*/tructorAnnotation/*=>>com.javacp.annot.ConstructorAnnotation#*/()(x/*<<=annot.B#`<init>`().(x)*//*<<=annot.B#(x)*/: Int/*=>>scala.Int#*/) {
  @ConstructorAnnotation/*=>>com.javacp.annot.ConstructorAnnotation#*/
  def this()/*<<=annot.B#`<init>`(+1).*/ = this(/*=>>annot.B#`<init>`().*/42)
}

@ObjectAnnotation/*=>>com.javacp.annot.ObjectAnnotation#*/
object /*=>>java.lang.Object#`<init>`().*/M/*<<=annot.M.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>annot.M.*/@MacroAnnotation/*=>>com.javacp.annot.MacroAnnotation#*/
  def m/*<<=annot.M.m().*/[TT/*<<=annot.M.m().(TT)*/]: Int/*=>>scala.Int#*//*=>>scala.Predef.`???`().*//*=>>scala.Predef.*//*=>>scala.*//*=>>_root_*/ = macro ???
}

@TraitAnnotation/*=>>com.javacp.annot.TraitAnnotation#*/
trait T/*<<=annot.T#*/

object /*=>>java.lang.Object#`<init>`().*/Alias/*<<=annot.Alias.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>annot.Alias.*/type A/*<<=annot.Alias.A#*/ = ClassAnnotation/*=>>com.javacp.annot.ClassAnnotation#*/ @param
}
