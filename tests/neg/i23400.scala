//> using options -preview
// preview needed for into in 3.8

import Conversion.into

import scala.deriving.Mirror

object Opaques:
  opaque into type MyInto[+A] >: A = A

import Opaques.MyInto

case class Foo(x: Int)
case class Bar(foo: into[Foo])
case class Baz(foo: MyInto[Foo])

given Conversion[Int, Foo] = Foo(_)

into enum Color: // ok
  case Red, Green

def test =
  val barMirror = summon[Mirror.Of[Bar]]
  summon[barMirror.MirroredElemTypes =:= (into[Foo] *: EmptyTuple.type)] // error
  summon[barMirror.MirroredElemTypes =:= (Foo *: EmptyTuple.type)] // ok

  val bazMirror = summon[Mirror.Of[Baz]]
  summon[bazMirror.MirroredElemTypes =:= (MyInto[Foo] *: EmptyTuple.type)] // ok
  summon[bazMirror.MirroredElemTypes =:= (Foo *: EmptyTuple.type)] // error