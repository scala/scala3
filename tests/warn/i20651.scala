package example

class B(val x: String)

class C:
  def wasBad = new B(getClass.getName){}.x // nowarn ever C.this.getClass
  def neverBad = new B(getClass.getName).x // nowarn ever

def alwaysBad = new B(getClass.getName).x // warn Predef.getClass not `package`.getClass

object A:
  def main(args: Array[String]): Unit =
    println(new B(getClass.getName){}.x) // nowarn was warn bc A is not $anon
    println(new B(getClass.getName).x) // nowarn A.getClass

trait T(val x: String)

object U:
  def main(args: Array[String]): Unit =
    println(new T(getClass.getName){}.x) // nowarn
