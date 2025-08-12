//> using options -Wunused:all

trait Test {
  def incr(): Unit
}

object Test {
  val test = new Test {
    var position: Int = 0 // warn position is assigned but not read

    def incr(): Unit = { position += 1 }
  }
}

class C:
  private var myvar: Int = 0 // warn for the same case with simpler syntax
  def mine: Int =
    myvar = 42
    27

class D:
  private var myvar: Int = 0 // nowarn (although read is RHS of assignment)
  def incr(): Unit = myvar = myvar + 1

  def local(): Unit =
    var localvar = 0 // warn local variable was mutated but not read
    localvar += 1

class E:
  trait Setting:
    def setting(): Unit
  val test = new Test:
    var settable: Int = 0 // warn private variable was mutated but not read
    def setting(): Unit =
      settable_=(42)
    def incr() = setting()

class F:
  private var myvar: Int = 0 // warn plain unused
