package example

import reflect.Selectable/*->scala::reflect::Selectable.*/.reflectiveSelectable/*->scala::reflect::Selectable.reflectiveSelectable().*/

object StructuralTypes/*<-example::StructuralTypes.*/:
  type User/*<-example::StructuralTypes.User#*/ = {
    def name/*<-local0*/: String/*->scala::Predef.String#*/
    def age/*<-local1*/: Int/*->scala::Int#*/
    def foo/*<-local3*/(x/*<-local2*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/
  }

  val user/*<-example::StructuralTypes.user.*/ = null.asInstanceOf/*->scala::Any#asInstanceOf().*/[User/*->example::StructuralTypes.User#*/]
  user/*->example::StructuralTypes.user.*/.name/*->scala::reflect::Selectable#selectDynamic().*/
  user/*->example::StructuralTypes.user.*/.age/*->scala::reflect::Selectable#selectDynamic().*/
  val fooBar/*<-example::StructuralTypes.fooBar.*/ = user/*->example::StructuralTypes.user.*/ foo/*->scala::reflect::Selectable#applyDynamic().*/ 123

  val V/*<-example::StructuralTypes.V.*/: Object/*->java::lang::Object#*/ {
    def scalameta/*<-local4*/: String/*->scala::Predef.String#*/
  } = new:
    /*<-local6*/def scalameta/*<-local5*/ = "4.0"
  V/*->example::StructuralTypes.V.*/.scalameta/*->scala::reflect::Selectable#selectDynamic().*/
end StructuralTypes/*->example::StructuralTypes.*/