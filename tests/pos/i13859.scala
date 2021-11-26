import scala.deriving.*

object Test:
  type Kind1[C, O[_]] = C {
    type MirroredType[X] = O[X]
    type MirroredMonoType = O[Any]
    type MirroredElemTypes[_] <: Tuple
  }

  type Kind2[C, O[_, _]] = C {
    type MirroredType[X, Y] = O[X, Y]
    type MirroredMonoType = O[Any, Any]
    type MirroredElemTypes[_, _] <: Tuple
  }

  type Test[X] = (X, Boolean)
  type Swap[X, Y] = (Y, X)

  locally {
    val x = summon[Kind1[Mirror.Product, Test]]
    x: Mirror.Product {
      type MirroredElemTypes[X] = (X, Boolean)
    }
  }

  locally {
    val x = summon[Kind2[Mirror.Product, Swap]]
    x: Mirror.Product {
      type MirroredElemTypes[X, Y] = (Y, X)
    }
  }
