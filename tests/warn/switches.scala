

import scala.annotation.switch

// this is testing not so much how things ought to be but how they are;
// the test is supposed to start failing if the behavior changes at all.
object Other {
  val C1 = 'P'                // fails: not final
  final val C2 = 'Q'          // succeeds: singleton type Char('Q') inferred
  final val C3: Char = 'R'    // fails: type Char specified
  final val C4 = '\u000A'     // succeeds like C2 but more unicodey
}

object Main {
  def succ1(c: Char) = (c: @switch) match {
    case 'A' | 'B' | 'C'  => true
    case 'd'              => true
    case 'f' | 'g'        => true
    case _                => false
  }

  def succ2(c: Char) = (c: @switch) match {
    case 'A' | 'B' | 'C'  => true
    case Other.C2         => true
    case Other.C4         => true
    case _                => false
  }

  // has a guard, but since SI-5830 that's ok
  // PENDING: #5070
  // def succ_guard(c: Char) = (c: @switch) match {
  //   case 'A' | 'B' | 'C'  => true
  //   case x if x == 'A'    => true
  //   case _                => false
  // }

  // throwing in @unchecked on the next two to make sure
  // multiple annotations are processed correctly

  // thinks a val in an object is constant... so naive
  def fail1(c: Char) = (c: @switch @unchecked) match { // warn: Could not emit switch for @switch annotated match
    case 'A'        => true
    case 'B'        => true
    case Other.C1   => true
    case _          => false
  }

  // more naivete
  def fail2(c: Char) = (c: @unchecked @switch) match { // warn: Could not emit switch for @switch annotated match
    case 'A'        => true
    case 'B'        => true
    case Other.C3   => true
    case _          => false
  }

  // guard case done correctly
  def succ3(c: Char) = (c: @switch) match {
    case 'A' | 'B' | 'C'  => true
    case x                => x == 'A'
  }

  // some ints just to mix it up a bit
  def succ4(x: Int, y: Int) = ((x + y): @switch) match {
    case  1 => 5
    case  2 => 10
    case  3 => 20
    case  4 => 50
    case 5|6|7|8 => 100
    case _  => -1
  }

  def fail3(x: Any) = (x: @switch) match { // warn: Could not emit switch for @switch annotated match
    case 1 | 2 | 3 => true
    case _ => false
  }

  def fail4(x: AnyVal) = (x: @switch) match { // warn: Could not emit switch for @switch annotated match
    case 1 | 2 | 3 => true
    case _ => false
  }

  case class IntAnyVal(x: Int) extends AnyVal

  val Ten = IntAnyVal(10)
  def fail5(x: IntAnyVal) = (x: @switch) match { // warn: Could not emit switch for @switch annotated match
    case IntAnyVal(1) => 0
    case Ten           => 1
    case IntAnyVal(100) => 2
    case IntAnyVal(1000) => 3
    case IntAnyVal(10000) => 4
  }

  // the generated lookupswitch covers only a subset of the cases
  final val One = IntAnyVal(1)
  def fail6(x: IntAnyVal) = (x: @switch) match { // warn: Could not emit switch for @switch annotated match
    case One          => 0
    case IntAnyVal(10) => 1
    case IntAnyVal(100) => 2
    case IntAnyVal(1000) => 3
    case IntAnyVal(10000) => 4
  }
}