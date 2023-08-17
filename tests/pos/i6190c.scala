//> using options -Xfatal-warnings -deprecation -feature

case class Rule(name: String)

def foo = List("1", "2").map(Rule.apply)
