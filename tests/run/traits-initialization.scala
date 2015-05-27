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

class D extends A {
 str += 'd'
 override val s = str += 'D'
 str += '3'
}

object Test extends D with B {
 // should only have 2 fields
 str += 'E'
 def main(args: Array[String]) = assert(str == "aA1dD3bB2E4")
 str += '4'
}
