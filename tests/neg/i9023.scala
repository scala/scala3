class C {
  var __= : Int = 42 // error
  var x_= : Int = 42 // error
}

class D {
  val __= : Int = 42 // error
  val x_= : Int = 42 // error
}

class E {
  lazy val __= : Int = 42 // error
  lazy val x_= : Int = 42 // error
}

class F {
  def __= : Int = 42
  def x_= : Int = 42
}
