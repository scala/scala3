package i26000merge

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object a extends File, caps.SharedCapability
object y extends File, caps.SharedCapability
object z extends File, caps.SharedCapability

// Typer merges the types of lambdas without comparing their capture sets, so the
// parameter types and bounds of merged lambdas are inferred. Other compilation units
// see them as empty. The parameter types and bounds of a single lambda are kept.
object Lib:
  def flag: Boolean = true
  val s = Box((x: File^{a, y}) => ())
  val sp = Box([C^ <: {a}] => (x: File^{C}) => ())
  val m = Box(if flag then (x: File^{a, y}) => () else (x: File^{a, z}) => ())
  val mp = Box(if flag then [C^ <: {a}] => (x: File^{C}) => () else [C^ <: {a, y}] => (x: File^{C}) => ())
  val ml = List((x: File^{a, y}) => (), (x: File^{a, z}) => ())
