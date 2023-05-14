import scala.quoted.*

trait Mirror:
  type ElemTypes <: Tuple

class Eq:

  def test0(using Quotes): Unit = '{
    type T
    ${ summonType[T]; ??? }
    ${ summonType[List[T]]; ??? }
  }

  def test1(using Quotes): Unit = '{
    val m: Mirror = ???
    ${ summonType[m.ElemTypes]; ??? }
    ${ summonType[List[m.ElemTypes]]; ??? }
  }

  def test2(using Quotes): Unit = '{
    val m: Mirror = ???
    type ET = m.ElemTypes
    ${ summonType[ET]; ??? }
    ${ summonType[List[ET]]; ??? }
  }

  def summonType[X](using Type[X]) = ???
