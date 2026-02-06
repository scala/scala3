// Test for issue #24201: Missing argument results in java.lang.VerifyError
// When an enum object extends a class with default parameters and uses
// named arguments out of order, invalid bytecode was generated.

// Test 1: Basic case - enum object with named args out of order
abstract class Foo[T](defaultValue: => T, arg1: Int = 1, arg2: Int = 2):
  def getValue: T = defaultValue
  def getArg1: Int = arg1
  def getArg2: Int = arg2

enum Baz:
  case E1, E2

object Baz extends Foo[Baz](Baz.E1, arg2 = 2)

// Test 2: Multiple by-name parameters with named args out of order
abstract class MultiBN[T](first: => T, second: => T, arg1: Int = 1, arg2: Int = 2):
  def getFirst: T = first
  def getSecond: T = second

enum Multi:
  case M1, M2

object Multi extends MultiBN[Multi](Multi.M1, Multi.M2, arg2 = 3)

// Test 3: Regular class (non-enum) companion object
class Regular(val name: String)

abstract class RegularParent[T](value: => T, x: Int = 1, y: Int = 2):
  def getValue: T = value

object Regular extends RegularParent[Regular](new Regular("test"), y = 5)

// Test 4: All parameters have defaults, all named args out of order
abstract class AllDefaults[T](a: => T, b: Int = 1, c: Int = 2, d: Int = 3):
  def getA: T = a
  def getB: Int = b
  def getC: Int = c
  def getD: Int = d

enum AllDef:
  case A1

object AllDef extends AllDefaults[AllDef](AllDef.A1, d = 10, c = 20, b = 30)

// Test 5: Self-reference in super call argument
abstract class SelfRef[T](value: => T, extra: Int = 0):
  def getValue: T = value

enum SelfRefEnum:
  case S1

// Self-reference where the companion module is accessed
object SelfRefEnum extends SelfRef[SelfRefEnum](SelfRefEnum.S1, extra = 42)

@main def Test =
  // Test 1: Basic enum case
  println(Baz.getValue)
  assert(Baz.getArg1 == 1, "Baz.getArg1 should be 1")
  assert(Baz.getArg2 == 2, "Baz.getArg2 should be 2")

  // Test 2: Multiple by-name parameters
  println(Multi.getFirst)
  println(Multi.getSecond)

  // Test 3: Regular class companion
  println(Regular.getValue.name)

  // Test 4: All defaults out of order
  println(AllDef.getA)
  assert(AllDef.getB == 30, "AllDef.getB should be 30")
  assert(AllDef.getC == 20, "AllDef.getC should be 20")
  assert(AllDef.getD == 10, "AllDef.getD should be 10")

  // Test 5: Self-reference
  println(SelfRefEnum.getValue)
