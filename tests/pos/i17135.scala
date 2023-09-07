package doobie

// original example
def someFunction(param: Int): Int = {
  sealed trait Foo {
    def asString: String = this match {
      case Foo.CaseC => "C"
    }
  }
  object Foo {
    // Having an object here crashes the compiler.
    object CaseC extends Foo
  }

  ???
}

// minimization
def foo =
  class Bar {
    // Having an object here crashes the compiler.
    lazy val CaseC =
      class Baz extends Foo
      new Baz()
  }
  val Bar: Bar = new Bar()
  trait Foo {
    def asString = Bar.CaseC
  }

// variant: outer is lazy val
lazy val lazyfoo =
  class Bar {
    // Having an object here crashes the compiler.
    lazy val CaseC =
      class Baz extends Foo
      new Baz()
  }
  val Bar: Bar = new Bar()
  trait Foo {
    def asString = Bar.CaseC
  }

// other example
def bar =
  sealed trait GADT2[A] extends Product with Serializable

  object GADT2 {
    case class IsDir(path: String) extends GADT2[_root_.scala.Boolean]
    case class Exists(path: String) extends GADT2[_root_.scala.Boolean]
    case class ReadBytes(path: String) extends GADT2[_root_.scala.Array[_root_.scala.Byte]]
    case class CopyOver(src: Seq[_root_.scala.Byte], path: String) extends GADT2[Int]
  }