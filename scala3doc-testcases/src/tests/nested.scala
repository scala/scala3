package tests.nested

class A
{
  object B
  {
    def bb(): Int
      = 123
    val a: Double
      = 0.3
  }

  class C
  {
    def cc: Int
      = 123
  }

  trait D
  {
    type AA = Int
  }
}

object R
{
  object B
  {
    def bb(): Int
      = 123
    val a: Double
      = 0.3
  }

  class C
  {
    def cc: Int
      = 123
  }

  trait D
  {
    type AA = Int
  }
}

class X
{
  object Y
}

// bug found in dotty code, still fails with type
sealed trait ErrorKind
object ErrorKind
{
  // This below produce some strange type
  // case object Parser extends ErrorKind
  //  case object Typer extends ErrorKind
}