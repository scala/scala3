//> using options -Werror -deprecation -feature

object Test {
  extension (x: Int)
    inline def times(inline op: Unit): Unit = {
      var count = 0
      while count < x do
        op
        count += 1
    }

  10.times { println("hello") }
}
