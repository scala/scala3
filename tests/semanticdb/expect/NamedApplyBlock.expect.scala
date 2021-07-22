package example

object NamedApplyBlockMethods/*<-example::NamedApplyBlockMethods.*/ {
  val local/*<-example::NamedApplyBlockMethods.local.*/ = 1
  def foo/*<-example::NamedApplyBlockMethods.foo().*/(a/*<-example::NamedApplyBlockMethods.foo().(a)*/: Int/*->scala::Int#*/ = 1, b/*<-example::NamedApplyBlockMethods.foo().(b)*/: Int/*->scala::Int#*/ = 2, c/*<-example::NamedApplyBlockMethods.foo().(c)*/: Int/*->scala::Int#*/ = 3): Int/*->scala::Int#*/ = a/*->example::NamedApplyBlockMethods.foo().(a)*/ +/*->scala::Int#`+`(+4).*/ b/*->example::NamedApplyBlockMethods.foo().(b)*/ +/*->scala::Int#`+`(+4).*/ c/*->example::NamedApplyBlockMethods.foo().(c)*/
  def baseCase/*<-example::NamedApplyBlockMethods.baseCase().*/ = foo/*->example::NamedApplyBlockMethods.foo().*/(local/*->example::NamedApplyBlockMethods.local.*/, c/*->example::NamedApplyBlockMethods.foo().(c)*/ = 3)
  def recursive/*<-example::NamedApplyBlockMethods.recursive().*/ = foo/*->example::NamedApplyBlockMethods.foo().*/(local/*->example::NamedApplyBlockMethods.local.*/, c/*->example::NamedApplyBlockMethods.foo().(c)*/ = foo/*->example::NamedApplyBlockMethods.foo().*/(local/*->example::NamedApplyBlockMethods.local.*/, c/*->example::NamedApplyBlockMethods.foo().(c)*/ = 3))
}

object NamedApplyBlockCaseClassConstruction/*<-example::NamedApplyBlockCaseClassConstruction.*/ {
  case class Msg/*<-example::NamedApplyBlockCaseClassConstruction.Msg#*/(body/*<-example::NamedApplyBlockCaseClassConstruction.Msg#body.*/: String/*->scala::Predef.String#*/, head/*<-example::NamedApplyBlockCaseClassConstruction.Msg#head.*/: String/*->scala::Predef.String#*/ = "default", tail/*<-example::NamedApplyBlockCaseClassConstruction.Msg#tail.*/: String/*->scala::Predef.String#*/)
  val bodyText/*<-example::NamedApplyBlockCaseClassConstruction.bodyText.*/ = "body"
  val msg/*<-example::NamedApplyBlockCaseClassConstruction.msg.*/ = Msg/*->example::NamedApplyBlockCaseClassConstruction.Msg.*/(bodyText/*->example::NamedApplyBlockCaseClassConstruction.bodyText.*/, tail/*->example::NamedApplyBlockCaseClassConstruction.Msg.apply().(tail)*/ = "tail")
}
