package annot

import com.javacp.annot.*
import scala.annotation.meta.*
import scala.language/*->scala::language.*/.experimental/*->scala::language.experimental.*/.macros/*->scala::language.experimental.macros.*/

@ClassAnnotation/*->com::javacp::annot::ClassAnnotation#*/
class Annotations/*<-annot::Annotations#*/[@TypeParameterAnnotation/*->com::javacp::annot::TypeParameterAnnotation#*/ T/*<-annot::Annotations#[T]*/](@ParameterAnnotation/*->com::javacp::annot::ParameterAnnotation#*/ x/*<-annot::Annotations#x.*/: T/*->annot::Annotations#[T]*/) { self/*<-local0*/: AnyRef/*->scala::AnyRef#*/ =>
  @FieldAnnotation/*->com::javacp::annot::FieldAnnotation#*/
  val field/*<-annot::Annotations#field.*/ = 42

  @MethodAnnotation/*->com::javacp::annot::MethodAnnotation#*/
  def method/*<-annot::Annotations#method().*/ = {
    @LocalAnnotation/*->com::javacp::annot::LocalAnnotation#*/
    val local/*<-local1*/ = 42
    local/*->local1*/
  }
  @TypeAnnotation/*->com::javacp::annot::TypeAnnotation#*/
  type S/*<-annot::Annotations#S#*/
}

class B/*<-annot::B#*/ @ConstructorAnnotation/*->com::javacp::annot::ConstructorAnnotation#*/()(x/*<-annot::B#x.*/: Int/*->scala::Int#*/) {
  @ConstructorAnnotation/*->com::javacp::annot::ConstructorAnnotation#*/
  def this/*<-annot::B#`<init>`(+1).*/() = this(42)
}

@ObjectAnnotation/*->com::javacp::annot::ObjectAnnotation#*/
object M/*<-annot::M.*/ {
  @MacroAnnotation/*->com::javacp::annot::MacroAnnotation#*/
  def m/*<-annot::M.m().*/[TT/*<-annot::M.m().[TT]*/]: Int/*->scala::Int#*/ = macro ???/*->scala::Predef.`???`().*/
}

@TraitAnnotation/*->com::javacp::annot::TraitAnnotation#*/
trait T/*<-annot::T#*/

object Alias/*<-annot::Alias.*/ {
  type A/*<-annot::Alias.A#*/ = ClassAnnotation/*->com::javacp::annot::ClassAnnotation#*/ @param/*->scala::annotation::meta::param#*/
}
