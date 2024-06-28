

case class Rule(name: String)

def foo = List("1", "2").map(Rule)  // warn

