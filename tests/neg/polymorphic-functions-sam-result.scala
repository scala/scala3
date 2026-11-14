// The body of a polymorphic function literal is created once and shared by all type
// applications, so it cannot be converted to a SAM type if creating an instance of it
// is observable.
abstract class MyAbstractSam[A]:
  def apply(x: A): A

trait Init[A]:
  println("init")
  def apply(x: A): A

trait WithVal[A]:
  val v = 1
  def apply(x: A): A

trait InitParent:
  println("init")
trait ViaParent[A] extends InitParent:
  def apply(x: A): A
trait ViaGrandParent[A] extends ViaParent[A]

trait UnivInit[A] extends Any:
  println("init")
  def apply(x: A): A

trait UnivInitParent extends Any:
  println("init")
trait ViaUnivParent[A] extends Any with UnivInitParent:
  def apply(x: A): A

abstract class Base
trait ExtendsClass[A] extends Base:
  def apply(x: A): A

class Out:
  trait Inner[A]:
    def apply(x: A): A
    def outer: Out = Out.this
  trait ExtendsInner[A] extends Inner[A]

def test(o: Out) =
  val f1: [A] => MyAbstractSam[A] = [A] => (x: A) => x // error
  val f2: [A] => Init[A] = [A] => (x: A) => x // error
  val f3: [A] => WithVal[A] = [A] => (x: A) => x // error
  val f4: [A] => ViaParent[A] = [A] => (x: A) => x // error
  val f5: [A] => ViaGrandParent[A] = [A] => (x: A) => x // error
  val f6: [A] => ExtendsClass[A] = [A] => (x: A) => x // error
  val f7: [A] => o.Inner[A] = [A] => (x: A) => x // error
  val f8: [A] => o.ExtendsInner[A] = [A] => (x: A) => x // error
  val f9: [A] => UnivInit[A] = [A] => (x: A) => x // error
  val f10: [A] => ViaUnivParent[A] = [A] => (x: A) => x // error
