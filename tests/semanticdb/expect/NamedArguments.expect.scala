package example

class NamedArguments/*<-example::NamedArguments#*/ {
  case class User/*<-example::NamedArguments#User#*/(name/*<-example::NamedArguments#User#name.*/: String/*->scala::Predef.String#*/)
  User/*->example::NamedArguments#User.*/(name = "John") // as there is no API to get the symbols of method arguments, this proposes a problem for how to generate references from a NamedArg
  User/*->example::NamedArguments#User.*/.apply/*->example::NamedArguments#User.apply().*/(name = "John") // as there is no API to get the symbols of method arguments, this proposes a problem for how to generate references from a NamedArg
}
