trait MyExtensions:
  extension (lhs: Int) def bash: Unit = {}
object MyExtensions extends MyExtensions

export MyExtensions.*
val fails = 1.bash
