package example

class NamedArguments/*<-example::NamedArguments#*/ {
  case class User/*<-example::NamedArguments#User#*/(name/*<-example::NamedArguments#User#name.*/: String/*->scala::Predef.String#*/)
  User/*->example::NamedArguments#User.*/(name/*->example::NamedArguments#User.apply().(name)*/ = "John")
  User/*->example::NamedArguments#User.*/.apply/*->example::NamedArguments#User.apply().*/(name/*->example::NamedArguments#User.apply().(name)*/ = "John")
}
