import scala.language.experimental.namedTuples
import NamedTuple.{AnyNamedTuple, NamedTuple}

trait Foo extends Selectable:
  val f: Any
  type Fields = (myfield: f.type)
  def selectDynamic(name: String): Any

object Test:
  val elem1: Foo { val f: Int } = ???
  def elem2: Foo { val f: Int } = ???

  def test: Unit =
    val a: Int = elem1.myfield // OK
    val b: Int = elem2.myfield // error: value myfield is not a member of Foo { val f: Int }
