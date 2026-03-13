//> using options -experimental

import scala.collection.Factory

@main def Test: Unit =
  val explicit: Factory[Int, IArray[Int]] = IArray.convertIArrayToFactory[Int](IArray)
  val contextual: Factory[Int, IArray[Int]] = IArray: Factory[Int, IArray[Int]]

  assert(explicit.getClass() == contextual.getClass()) // default is same implementation

  val convertFromList = List(1,2,3).to(IArray)
  val ev: IArray[Int] = convertFromList

  assert(convertFromList.getClass == IArray(1,2,3).getClass())

  assert(ev.sameElements(Vector(1,2,3)))
