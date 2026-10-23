// Polymorphic function literals whose body is converted to a SAM type. The body is
// created once and shared by all type applications, which is unobservable since
// creating an instance of these SAM types runs no code.
import scala.collection.mutable.ListBuffer

trait MySam[A]:
  def apply(x: A): A

// needs a bridge, so it is implemented by an anonymous class on the JVM
trait GenSam[T]:
  def apply(x: T): T
trait IntSam extends GenSam[Int]:
  def apply(x: Int): Int

// concrete methods and a pure parent trait
trait Helper:
  def twice(x: Int): Int = 2 * x
trait WithHelper[A] extends Helper:
  def apply(x: A): Int

// the constructor proxy of `C` does not exist at run time
trait WithClass[A]:
  class C
  def apply(x: A): A

class Outer:
  trait Inner[A]: // a pure interface needs no outer reference
    def apply(x: A): A
  val inner: [A] => Inner[A] = [A] => (x: A) => x

@main def Test =
  val log = ListBuffer[String]()

  val f1: [A] => MySam[A] = [A] => (x: A) => { log += "f1"; x }
  val g1 = f1[Int]
  log += "applied"
  assert(g1(1) == 1 && f1[String]("a") == "a")
  assert(log.toList == List("applied", "f1", "f1"))

  val f2: [A] => IntSam = [A] => (x: Int) => x + 1
  assert(f2[String](1) == 2)
  assert((f2[String]: GenSam[Int])(1) == 2)

  val f3: [A] => WithHelper[A] = [A] => (x: A) => x.toString.length
  assert(f3[Int](100) == 3 && f3[Int].twice(2) == 4)

  val f4: [A] => Runnable = [A] => () => log += "run"
  f4[Int].run()
  assert(log.last == "run")

  val f5: [A] => java.util.function.Function[A, List[A]] = [A] => (x: A) => List(x)
  assert(f5[Int].apply(1) == List(1))

  val f6: [A] => PartialFunction[A, Int] = [A] => (x: A) => x match { case i: Int => i }
  assert(f6[Any].isDefinedAt(1) && !f6[Any].isDefinedAt("a"))
  assert(f6[Any].orElse { case _ => 0 }("a") == 0)

  assert(Outer().inner[Int](1) == 1)

  val f7: [A] => WithClass[A] = [A] => (x: A) => x
  assert(f7[Int](1) == 1)
