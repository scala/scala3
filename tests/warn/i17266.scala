//> using options  -explain

def test1 =
  synchronized { // warn
    println("hello")
  }

def test2 =
  this.synchronized { // not an error (should be?)
    println("hello")
  }

object MyLib

def test3 =
  import MyLib.*
  synchronized { // warn
    println("hello")
  }

def test4 =
  1.synchronized { // warn
    println("hello")
  }

object Test4:
  synchronized { // not an error
    println("hello")
  }

object Test5:
  def test5 =
    synchronized { // not an error
      println("hello")
    }

object Test6:
  import MyLib.*
  synchronized { // not an error
    println("hello")
  }

object Test7:
  import MyLib.*
  def test7 =
    synchronized { // not an error
      println("hello")
    }

/*
object Test7b:
  def test8 =
    import MyLib.*
    synchronized { // already an error: Reference to synchronized is ambiguous.
      println("hello")
    }
*/

class Test8:
  synchronized { // not an error
    println("hello")
  }

class Test9:
  def test5 =
    synchronized { // not an error
      println("hello")
    }

class Test10:
  import MyLib.*
  synchronized { // not an error
    println("hello")
  }

class Test11:
  import MyLib.*
  def test7 =
    synchronized { // not an error
      println("hello")
    }

trait Test12:
  synchronized { // not an error
    println("hello")
  }

trait Test13:
  def test5 =
    synchronized { // not an error
      println("hello")
    }

trait Test14:
  import MyLib.*
  synchronized { // not an error
    println("hello")
  }

trait Test15:
  import MyLib.*
  def test7 =
    synchronized { // not an error
      println("hello")
    }

def test16 =
  wait() // warn

def test17 =
  this.wait() // not an error (should be?)

def test18 =
  import MyLib.*
  wait() // warn

def test19 =
  1.wait() // not an error (should be?)

def test20 =
  wait(10) // warn

def test21 =
  this.wait(10) // not an error (should be?)

def test22 =
  import MyLib.*
  wait(10) // warn

def test23 =
  1.wait(10) // not an error (should be?)

def test24 =
  hashCode() // warn

def test25 =
  this.hashCode() // not an error (should be?)

def test26 =
  import MyLib.*
  hashCode() // warn

def test27 =
  1.hashCode()// not an error (should be? probably not)