trait A {
 var str = ""
 str = "a"
 val s = str += 'A'
 str += '1'
}

trait B extends A {
 str += 'b'
 override val s = str += 'B'
 str += '2'
}

class D(sup: =>String) extends A {
 str += 'd'
 override val s = str += 'D'
 str += '3'
}

object Test extends D({Test.str += "Z"; Test.str}) with B {
 // should only have 2 fields
 str += 'E'
 def main(args: Array[String]) = assert(str == "aA1dD3bB2E4", str)
 str += '4'
}
