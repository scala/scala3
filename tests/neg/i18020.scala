import _root_.scala.StringContext // ok

class Test :
  val Foo = 1
  def foo0: Unit =
    val x = new _root_.scala.StringContext() // ok
    val y: Option[_root_.scala.Serializable] = None // ok
    val z: _root_.scala.None.type = None
    val w = _root_.scala.None
    val (_root_, other) = (1, 2) // error
    val (Test.this.Foo, 1) = ???
    ??? match
      case (Test.this.Foo, 1) => ()

def foo3 =
  val _root_ = "abc" // error

def foo1: Unit =
  val _root_: String = "abc" // error // error
  // _root_: is, technically, a legal name
  // so then it tries to construct the infix op pattern
  // "_root_ String .." and then throws in a null when it fails
  // to find an argument
  // then Typer rejects "String" as an infix extractor (like ::)
  // which is the second error

def foo2: Unit = // was: error, recursive value _root_ needs type
  val _root_ : String = "abc" // error

// i17757
def fooVal: Unit =
  val _root_  = "abc" // error
  println(_root_.length)  // error
  println(_root_)  // error

def barVal: Unit =
  _root_  // error
  _root_.scala  // error
  println(_root_)  // error
  println(_root_.scala)  // error

// i18050
package p {
  package _root_ { // error
    object X // error
  }
}

// scala/bug#12508
package _root_ { // ok
  class C {
    val _root_ = 42 // error
  }
}
package _root_.p { // ok
  class C
}

// from ScalaPB
def fromScalaPb(x: Option[String]) = x match
  case _root_.scala.Some(s) => s
  case _                    => ""
