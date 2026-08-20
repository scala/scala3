sealed trait Updater
final class Initializer extends Updater
final class Mutator extends Updater

final class IArray[+A]
val Empty = new IArray[Nothing]

def partitionCollect2[A, A1, A2](
    as: IArray[A],
    t1: PartialFunction[A, A1],
    t2: PartialFunction[A, A2],
): (IArray[A1], IArray[A2], IArray[A]) =
  ???

def test(updaters: IArray[Updater]) =
  val (mutators: IArray[Mutator], initializers: IArray[Initializer], Empty) =
    partitionCollect2(
      updaters,
      { case x: Mutator     => x },
      { case x: Initializer => x },
    )
