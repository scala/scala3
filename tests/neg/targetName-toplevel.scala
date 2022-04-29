import scala.annotation.targetName

@targetName("B1") class A1  // error targetName on top-level class
@targetName("B2") trait A2  // error targetName on top-level trait
@targetName("B3") object A3 // error targetName on top-level object

@targetName("bar") def foo = 42 // OK

object Outer:
  @targetName("B1") class A1  // OK
  @targetName("B2") trait A2  // OK
  @targetName("B3") object A3 // OK
  @targetName("D1") class C1  // OK
  @targetName("D2") trait C2  // OK
  @targetName("D3") object C3 // OK

export Outer.{A1, A2, A3}  // error // error // error already defined
export Outer.{C1, C2, C3}  // OK
