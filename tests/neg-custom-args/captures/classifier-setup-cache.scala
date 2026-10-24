import caps.{any, Classifier, SharedCapability, Control}

// Setup sees members before their types are transformed. Classifiers computed
// there must not be reused when checking.

trait C extends SharedCapability, Classifier
trait D extends C, Classifier
class Lbl extends Control:
  def break(): Unit = ()
class H(l: Lbl^):
  def go(): Unit = l.break()
class IO extends SharedCapability
class A(val io: IO^)

// a union of capturing types
class K1(l: Lbl^, z: Object^):
  val x = H(l)
  def f(b: Boolean): H^{this.z.only[C]} | H^{this.x.only[D]} = ???
  def g(): H^{this.x.except[Control]} = this.x // error

// an aliased capturing type
class K2(l: Lbl^, z: Object^):
  val x = H(l)
  type T = H^{this.z.only[C]}
  def f(): T^{this.x.only[D]} = ???
  def g(): H^{this.x.except[Control]} = this.x // error

class K3(io: IO^, z: Object^):
  val x = A(io)
  type T = A^{this.z.only[C]}
  def f(): T^{this.x.only[D]} = ???
  def g(): A^{this.x.only[C]} = this.x // error

class K4(io: IO^):
  type T = A^{this.a.only[C]}
  def f(): T^{this.a.except[C]} = ???
  def g(): A^{this.a.only[C]} = this.a // error
  val a = A(io)

// local values
def m(l: Lbl^, z: Object^) =
  val x = H(l)
  def f(b: Boolean): H^{z.only[C]} | H^{x.only[D]} = ???
  def g(): H^{x.except[Control]} = x // error
  ()
