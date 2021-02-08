object Test {
  import Lib.*
  val xs: IArray2[Int] = IArray2(1)
}

object Lib {
  opaque type IArray2[+T] = Array[_ <: T]

  object IArray2 {
    inline def apply(x: =>Int): IArray2[Int] = Array(x) // error
  }
}
