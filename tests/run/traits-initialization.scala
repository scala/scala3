object store {
  var str = ""
}

import store._

trait A {
 str += "a"
 val s = str += 'A'
 str += '1'
}

trait B extends A {
 str += 'b'
 override val s = str += 'B'
 str += '2'
}

class D(sup: Unit) extends A {
 str += 'd'
 override val s = str += 'D'
 str += '3'
}


object Test extends D({str += "Z"}) with B {
 // should only have 2 fields
 str += 'E'
 def main(args: Array[String]) = assert(str == "ZaA1dD3bB2E4", str)
 str += '4'
}
