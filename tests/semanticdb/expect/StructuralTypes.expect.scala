package example

import reflect.Selectable/*->scala::reflect::Selectable.*/.reflectiveSelectable/*->scala::reflect::Selectable.reflectiveSelectable().*/

object StructuralTypes/*<-example::StructuralTypes.*/:
  type User/*<-example::StructuralTypes.User#*/ = {
    def name/*<-example::StructuralTypes.User#name().*/: String/*->scala::Predef.String#*/
    def age/*<-example::StructuralTypes.User#age().*/: Int/*->scala::Int#*/
  }

  type FooUser/*<-example::StructuralTypes.FooUser#*/ = User/*->example::StructuralTypes.User#*/ {
    def foo/*<-example::StructuralTypes.FooUser#foo().*/(x/*<-example::StructuralTypes.FooUser#foo().(x)*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/
  }

  val user/*<-example::StructuralTypes.user.*/ = null.asInstanceOf/*->scala::Any#asInstanceOf().*/[FooUser/*->example::StructuralTypes.FooUser#*/]
  user/*->example::StructuralTypes.user.*/.name/*->example::StructuralTypes.User#name().*/
  user/*->example::StructuralTypes.user.*/.age/*->example::StructuralTypes.User#age().*/
  val fooBar/*<-example::StructuralTypes.fooBar.*/ = user/*->example::StructuralTypes.user.*/ foo/*->example::StructuralTypes.FooUser#foo().*/ 123
  val V/*<-example::StructuralTypes.V.*/: Object/*->java::lang::Object#*/ {
    def scalameta/*<-local0*/: String/*->scala::Predef.String#*/
  } = /*<-local2*/new:
    def scalameta/*<-local1*/ = "4.0"
  V/*->example::StructuralTypes.V.*/.scalameta/*->local4*/
end StructuralTypes/*->example::StructuralTypes.*/