package example

class Bonjour() {
}

class Bonjour2(val x:Int) {
  def this(x:String) = this(2)
}

class TestNew extends C {
  val b = new B
  val c = new Bonjour
  val d = new Bonjour()
  val e = new Bonjour2(2)
  val f = new Bonjour2("a")
  val _ = f.x
}