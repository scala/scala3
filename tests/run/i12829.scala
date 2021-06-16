object Test:

  class Foo:
    var buf = ""

    def add(v: String): Unit = buf += v

    inline def += (inline v: String): this.type = {add(v); this }


  def main(sa: Array[String]): Unit =

    var fooVar = new Foo
    fooVar += "a" += "b" += "c"
    println(fooVar.buf)              // Prints: abc

    val fooVal = new Foo
    fooVal += "a" += "b" += "c"
    println(fooVal.buf)              // Printed: c     // Expected abc
