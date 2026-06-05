package example

import reflect.Selectable/*->scala::reflect::Selectable.*/.reflectiveSelectable/*->scala::reflect::Selectable.reflectiveSelectable().*/

object StructuralTypesParams/*<-example::StructuralTypesParams.*/:
  type User/*<-example::StructuralTypesParams.User#*/ = {
    def foo/*<-example::StructuralTypesParams.User#foo().*/(arg1/*<-example::StructuralTypesParams.User#foo().(arg1)*/: Int/*->scala::Int#*/, arg2/*<-example::StructuralTypesParams.User#foo().(arg2)*/: String/*->scala::Predef.String#*/): String/*->scala::Predef.String#*/
    def age/*<-example::StructuralTypesParams.User#age().*/: Int/*->scala::Int#*/

  }
  val user/*<-example::StructuralTypesParams.user.*/ = null.asInstanceOf/*->scala::Any#asInstanceOf().*/[User/*->example::StructuralTypesParams.User#*/]
  val num/*<-example::StructuralTypesParams.num.*/ = 123
  val str/*<-example::StructuralTypesParams.str.*/ = "abc"
  val fooBar/*<-example::StructuralTypesParams.fooBar.*/ = user/*->example::StructuralTypesParams.user.*/.foo/*->example::StructuralTypesParams.User#foo().*/(num/*->example::StructuralTypesParams.num.*/, str/*->example::StructuralTypesParams.str.*/)
  val fooBaz/*<-example::StructuralTypesParams.fooBaz.*/ = user/*->example::StructuralTypesParams.user.*/.foo/*->example::StructuralTypesParams.User#foo().*/(arg1 = num/*->example::StructuralTypesParams.num.*/, arg2 = str/*->example::StructuralTypesParams.str.*/)
  val age/*<-example::StructuralTypesParams.age.*/ = user/*->example::StructuralTypesParams.user.*/.age/*->example::StructuralTypesParams.User#age().*/

end StructuralTypesParams/*->example::StructuralTypesParams.*/