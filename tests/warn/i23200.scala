//> using options -Wunused:all

trait Foo
trait Bar

def `anon not updated` =
  new Foo {
    var x: Int = 42 // warn
    val _ = new Bar:
      println(x)
    //x = 27
    //x_=(27)
  }
def `anon yes updated` =
  new Foo {
    var x: Int = 42 // nowarn
    val _ = new Bar:
      println(x)
    x = 27
    //x_=(27)
  }
def `anon yes updated from nested context` =
  new Foo {
    var x: Int = 42 // nowarn
    val _ = new Bar:
      println(x)
      x = 27
    //x_=(27)
  }
def `anon yes updated in daring use of setter` =
  new Foo {
    var x: Int = 42 // nowarn
    val _ = new Bar:
      println(x)
    //x = 27
    x_=(27)
  }

def f: Unit =
  var x: Int = 42 // warn local var
  println(x)
