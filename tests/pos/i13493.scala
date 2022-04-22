import deriving.Mirror

sealed trait Box[T]
object Box

case class Child[T](t: T) extends Box[T]

object MirrorK1:
  type Of[F[_]] = Mirror { type MirroredType[A] = F[A] }

def testSums =

  val foo = summon[Mirror.Of[Option[Int] | Option[String]]]
  summon[foo.MirroredElemTypes =:= (None.type, Some[Int] | Some[String])]

  val bar = summon[Mirror.Of[Box[Int] | Box[String]]]
  summon[bar.MirroredElemTypes =:= ((Child[Int] | Child[String]) *: EmptyTuple)]

  val qux = summon[Mirror.Of[Option[Int | String]]]
  summon[qux.MirroredElemTypes =:= (None.type, Some[Int | String])]

  val bip = summon[Mirror.Of[Box[Int | String]]]
  summon[bip.MirroredElemTypes =:= (Child[Int | String] *: EmptyTuple)]

  val bap = summon[MirrorK1.Of[[X] =>> Box[X] | Box[Int] | Box[String]]]
  summon[bap.MirroredElemTypes[Boolean] =:= ((Child[Boolean] | Child[Int] | Child[String]) *: EmptyTuple)]


def testProducts =
  val foo = summon[Mirror.Of[Some[Int] | Some[String]]]
  summon[foo.MirroredElemTypes =:= ((Int | String) *: EmptyTuple)]

  val bar = summon[Mirror.Of[Child[Int] | Child[String]]]
  summon[bar.MirroredElemTypes =:= ((Int | String) *: EmptyTuple)]

  val qux = summon[Mirror.Of[Some[Int | String]]]
  summon[foo.MirroredElemTypes =:= ((Int | String) *: EmptyTuple)]

  val bip = summon[Mirror.Of[Child[Int | String]]]
  summon[bip.MirroredElemTypes =:= ((Int | String) *: EmptyTuple)]

  val bap = summon[MirrorK1.Of[[X] =>> Child[X] | Child[Int] | Child[String]]]
  summon[bap.MirroredElemTypes[Boolean] =:= ((Boolean | Int | String) *: EmptyTuple)]
