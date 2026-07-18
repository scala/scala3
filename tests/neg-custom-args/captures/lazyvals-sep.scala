import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Ref(x: Int) extends Mutable:
  private var value: Int = x
  def get(): Int = value
  update def set(newValue: Int): Unit = value = newValue

// For testing types other than functions
class Wrapper(val ref: Ref^) extends Stateful:
  def compute(): Int = ref.get()
  update def mutate(x: Int): Unit = ref.set(x)

class WrapperRd(val ref: Ref^{any.rd}):
  def compute(): Int = ref.get()

class TestClass extends Stateful:
  val r: Ref^ = Ref(0)
  val r2: Ref^ = Ref(42)

  // Test case 1: Read-only access in initializer - should be OK
  lazy val lazyVal: () ->{r.rd} Int =
    val current = r2.get()
    () => r.get() + current

  // Test case 2: Exclusive access in initializer - should error
  lazy val lazyVal2: () ->{r.rd} Int =
    val current = r2.get()
    r.set(current * 100)  // error - exclusive access in initializer
    () => r.get()

  // Test case 3: Exclusive access in returned closure - should be OK
  lazy val lazyVal3: () ->{r} Int =
    val current = r2.get()
    () => r.set(current); r.get()  // error, even though exclusive access is in closure, not initializer

  // Test case 4: Multiple nested blocks with exclusive access - should error
  lazy val lazyVal4: () ->{r.rd} Int =
    val x = {
      val y = {
        r.set(100)  // error
        r.get()
      }
      y + 1
    }
    () => r.get() + x

  // Test case 5: Multiple nested blocks with only read access - should be OK
  lazy val lazyVal5: () ->{r.rd} Int =
    val x = {
      val y = {
        r.get()  // only read access in nested blocks
      }
      y + 1
    }
    () => r.get() + x

  // Test case 6: WrapperRd type - exclusive access in initializer - should error
  lazy val lazyVal6: WrapperRd^{r} =
    r.set(200)  // error
    WrapperRd(r)

  // Test case 7: WrapperRd type - non-exclusive access in initializer - should be ok
  lazy val lazyVal7: WrapperRd^{r} =
    r.get()
    WrapperRd(r)

  // Test case 8: Wrapper type - exclusive access in initializer - should error
  lazy val lazyVal8: Wrapper^{any, r} =
    r.set(200)  // error
    Wrapper(r)  // error

  // Test case 9: Wrapper type - non-exclusive access in initializer - should error
  lazy val lazyVal9: Wrapper^{any, r} =
    r.get()
    Wrapper(r) // error

  // Test case 10: Conditional with exclusive access - should error
  lazy val lazyVal10: WrapperRd^{r} =
    val x = if r.get() > 0 then
      r.set(0)  // error exclusive access in conditional
      1
    else 2
    WrapperRd(r)

  // Test case 11: Try-catch with exclusive access - should error
  lazy val lazyVal11: () ->{r.rd} Int =
    val x = try
      r.set(42)  // error
      r.get()
    catch
      case _: Exception => 0
    () => r.get() + x

  // Test case 12: Local exclusive access - should be OK
  lazy val lazyVal12: () => Int =
    val r3: Ref^ = Ref(10)
    r3.set(100)  // ok
    val i = r3.get()
    () => i

  // Test case 13: Local exclusive access - should be OK
  lazy val lazyVal13: () => Int =
    val r3: Ref^ = Ref(10)
    r3.set(100)  // ok
    () => r3.get()

  // Test case 14: Local exclusive access - should be OK
  lazy val lazyVal14: () => Ref^ =
    val r3: Ref^ = Ref(10)
    r3.set(100)  // ok
    () => r3

def test =
  val r: Ref^ = Ref(0)
  val r2: Ref^ = Ref(42)

  lazy val lazyVal2: () ->{r.rd} Int =
    val current = r2.get()
    r.set(current * 100)  // ok, lazy vals outside Stateful can access exclusive capabilities
    () => r.get()

