// scalac: -Werror -explain

def test1 =
  synchronized { // error
    println("hello")
  }

def test2 =
  this.synchronized { // error
    println("hello")
  }

object MyLib

def test3 =
  import MyLib.*
  synchronized { // not an error (should be?)
    println("hello")
  }

def test4 =
  wait() // error

def test5 =
  this.wait() // error

def test6 =
  import MyLib.*
  wait() // not an error (should be?)

def test7 =
  wait(10) // error

def test8 =
  this.wait(10) // error

def test9 =
  import MyLib.*
  wait(10) // not an error (should be?)

def test10 =
  hashCode() // error

def test11 =
  this.hashCode() // error

def test12 =
  import MyLib.*
  hashCode() // not an error (should be?)
