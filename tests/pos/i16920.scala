
object One:
  extension (s: String)
    def wow: Unit = println(s)

object Two:
  extension (i: Int)
    def wow: Unit = println(i)

object Three:
  extension (s: String)
    def wow: Unit = println(s)
  extension (i: Int)
    def wow: Unit = println(i)

object Four:
  implicit class WowString(s: String):
    def wow: Unit = println(s)

object Five:
  implicit class WowInt(i: Int):
    def wow: Unit = println(i)

object Compiles:
  import Three._
  def test: Unit =
    5.wow
    "five".wow

object AlsoCompiles:
  import Four._
  import Five._
  def test: Unit =
    5.wow
    "five".wow

object UsedToFail:
  import One._
  import Compiles.*
  import Two._
  def test: Unit =
    5.wow
    "five".wow

object Conflicting:
  extension (i: Int)
    def wow: Unit = println(i)

object Named:
  import One.wow
  import Two.wow
  import Conflicting._
  def test: Unit =
    5.wow // ok
    "five".wow // ok

object Named2:
  import Conflicting._
  import One.wow
  import Two.wow
  def test: Unit =
    5.wow // ok
    "five".wow // ok

val Alias = Two

object Named3:
  import Alias._
  import Two._
  def test: Unit =
    5.wow // ok

object Named4:
  import Two._
  import Alias._
  def test: Unit =
    5.wow // ok
