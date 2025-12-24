//> using options -Werror -deprecation -feature

case class Rule(name: String)
object Rule extends (String => Rule) {
  def apply(name: String): Rule = new Rule(name)
}

def foo = List("1", "2").map(Rule)
