object store {
  var str = ""
}

import store.*

trait A {
 str += "a"
 val i = 2
 val s = str += 'A'
 str += '1'
}

trait B extends A {
 str += 'b'
 override val i = 3
 override val s = str += 'B'
 str += '2'
}

class D(sup: Unit) extends A {
 str += 'd'
 override val i = 4
 override val s = str += 'D'
 str += '3'
}

class E(sup: Unit) extends A with B {
 str += 'd'
 override val i = 5
 override val s = str += 'E'
 str += '3'
}


object Test extends D({str += "Z"}) with B {
 // should only have 2 fields
 str += 'E'
 def main(args: Array[String]) = assert(str == "ZaA1dD3bB2E4", str)
 str += '4'
}
