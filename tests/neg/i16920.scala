import language.experimental.relaxedExtensionImports

object One:
  extension (s: String)
    def wow: Unit = println(s)

object Two:
  extension (i: Int)
    def wow: Unit = println(i)

object Three:
  extension (i: Int)
    def wow: Unit = println(i)

object Fails:
  import One._
  def test: Unit =
    import Two._
    5.wow
    "five".wow // error

object AlsoFails:
  extension (s: Boolean)
    def wow = println(s)
  import One._
  import Two._
  def test: Unit =
    5.wow // error
    "five".wow // error

object Fails2:
  import One._
  import Two._
  import Three._
  def test: Unit =
    5.wow // error
    "five".wow // ok

object Fails3:
  import One._
  import Two.wow
  def test: Unit =
    5.wow // ok
    "five".wow // error

object Fails4:
  import Two.wow
  import One._
  def test: Unit =
    5.wow // ok
    "five".wow // error

object Fails5:
  import One.wow
  import Two.wow
  import Three.wow
  def test: Unit =
    5.wow // error
    "five".wow // ok