package i26000leftout

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object a extends File, caps.SharedCapability
object b extends File, caps.SharedCapability
object c extends File, caps.SharedCapability

// Capture sets in invariant positions of inferred types are solved at their current
// elements. Other compilation units and later REPL lines see these types as well, so
// the capture sets are final, and capabilities that the right hand side would allow
// are left out of the inferred type.
object Lib:
  def flag: Boolean = true
  val f1 = (x: File^{a, b}) => ()
  val f2 = (x: File^{a, c}) => ()
  def mk(y: File^) = (x: File^{y}) => ()

  val merged = Box(if flag then (x: File^{a, b}) => () else (x: File^{a, c}) => ()) // warn: leaves out a
  def viaMethod(y: File^) = Box(mk(y)) // warn: leaves out y
  def mergedParam(y: File^) = Box(if flag then (x: File^{y}) => () else (x: File^{y, a}) => ()) // warn: leaves out y
  val mergedPoly = Box(if flag then [C^] => (x: File^{C}) => () else [C^] => (x: File^{C}) => ()) // warn: leaves out C

  // The parameter types of single lambdas and of definitions are kept
  val single = Box((x: File^{a}) => ())
  val copied = Box(f1)
  // Nothing is left out
  val explicit: Box[File^{a} => Unit] = Box(if flag then (x: File^{a, b}) => () else (x: File^{a, c}) => ())
  val disjoint = Box(if flag then (x: File^{b}) => () else (x: File^{c}) => ())
  val unbounded = Box(File())
  // Contravariant capture sets are widened to their upper approximation
  val covariant = List(f1, f2)

class Open:
  // Other units can override `h` with a function that accepts only pure files,
  // so its parameter type is not widened
  val h = if Lib.flag then Lib.f1 else Lib.f2 // warn: leaves out a
  def g = if Lib.flag then Lib.f1 else Lib.f2 // warn: leaves out a
  final val hf = if Lib.flag then Lib.f1 else Lib.f2
  private val hp = Box(if Lib.flag then Lib.f1 else Lib.f2)
  def useHp(): Unit = hp.value(File())

// Local definitions do not warn, since all their uses are in this unit
object Local:
  def local(): Unit =
    val l = Box(if Lib.flag then Lib.f1 else Lib.f2)
    l.value(File())
