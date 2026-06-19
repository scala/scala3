//> using options -feature -Werror
//                ^^^^^^^^
//                |        ^^^^^^^^
//                |        |
//                |        ensure error warnings
//                ensure warning for implicit conversions

import scala.collection.Factory

@main def Test: Unit =
  val explicitConversion: Factory[Int, IArray[Int]] = IArray.convertIArrayToFactory[Int](IArray)
  val implicitConversion: Factory[Int, IArray[Int]] = IArray: Factory[Int, IArray[Int]]

   // default is same implementation
  assert(explicitConversion.getClass() == implicitConversion.getClass())

  locally {
    // use case 1: convert IArray companion object to Factory for explicit use.
    val convertFromList = List(1,2,3).to(IArray)
    val ev: IArray[Int] = convertFromList

    assert(convertFromList.getClass == IArray(1,2,3).getClass())

    assert(ev.sameElements(Vector(1,2,3)))
  }

  locally {
    // use case 2: summon factory from the context

    var capturedFactory: Factory[?, ?] = null
    def makeElems[T, CC[_]](elems: T*)(using CC: Factory[T, CC[T]]): CC[T] =
      capturedFactory = CC
      elems.to(CC)

    val foo: IArray[Int] = makeElems(1,2,3)
    assert(capturedFactory.getClass == explicitConversion.getClass)
  }
