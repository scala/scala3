import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*
class Ref extends Mutable:
  update def set = ???
def swap(a: Ref^, b: Ref^): Unit = ()
def test1(): Unit =
  val a = Ref()
  val b = Ref()
  val f1: (x: Ref^) -> List[() ->{a,x} Unit] = ???
  f1(a)  // error, as expected
  val f2: (x: Ref^) -> List[() ->{x} Unit] = ???
  f2(a)  // ok, as expected
  val f3: (x: Ref^) -> (op: () ->{b} Unit) -> List[() ->{op} Unit] = ???
  f3(a)  // ok
  f3(b)  // error
  val f4: (x: Ref^) -> (y: Ref^{x}) ->{x} Unit = ???
  f4(a)  // ok
  val f5: (x: Ref^) -> (y: List[Ref^{a}]) ->{} Unit = ???
  f5(a)  // ok
  val f6: (x: Ref^) -> (y: List[Ref^{a, b}]) ->{} Unit = ???
  f6(b)  // ok
  val f7: (x: Ref^) ->{a, b} (y: List[Ref^{a, b}]) ->{a, b} Unit = ???
  f7(a)  // error
