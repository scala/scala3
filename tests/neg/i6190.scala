class Rule(name: String)
object Rule {
  def apply(name: String): Rule = new Rule(name)
}

def foo = List("1", "2").map(Rule)  // error
