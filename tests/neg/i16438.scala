//> using options -Wsafe-init
trait ATrait(val string: String, val int: Int)
trait AnotherTrait( override val string: String, override val int: Int) extends ATrait
case class ACaseClass(override val string: String) extends AnotherTrait(string, 3) // error
