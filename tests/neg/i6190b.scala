//> using options -Xfatal-warnings

case class Rule(name: String)

def foo = List("1", "2").map(Rule)  // warn

// nopos-error: No warnings can be incurred under -Werror.
