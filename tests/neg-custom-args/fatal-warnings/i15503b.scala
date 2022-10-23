// scalac: -Wunused:locals

val a = 1 // OK

val b = // OK
  val e1 = 1 // error
  def e2 = 2 // error
  1

val c = // OK
  val e1 = 1 // OK
  def e2 = e1 // OK
    e2

def d = 1 // OK

def e = // OK
  val e1 = 1 // error
  def e2 = 2 // error
  1

def f = // OK
  val f1 = 1 // OK
  def f2 = f1 // OK
  f2

class Foo {
  val b = // OK
    val e1 = 1 // error
    def e2 = 2 // error
    1

  val c = // OK
    val e1 = 1 // OK
    def e2 = e1 // OK
      e2

  def d = 1 // OK

  def e = // OK
    val e1 = 1 // error
    def e2 = 2 // error
    1

  def f = // OK
    val f1 = 1 // OK
    def f2 = f1 // OK
    f2
}
