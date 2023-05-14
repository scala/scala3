@deprecated("no CaseClass")
case class CaseClass(rgb: Int):
  def magic(): Unit = ()

object CaseClass:
  def notDeprecated(): Unit = ()

val a: CaseClass = CaseClass(42)        // error: deprecated type // error: deprecated apply method
val b: CaseClass = new CaseClass(42)    // error: deprecated type // error: deprecated class
val c: Unit = CaseClass(42).magic()     // error: deprecated apply method
val d: Unit = CaseClass.notDeprecated() // compiles
