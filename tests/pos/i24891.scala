object Main {
  def test1: Unit = {
    val x1 = IArray("a")
    val x2 = IArray(x1)
    val x3: IArray[IArray[String]] = x2
  }
  def test2a: Unit = {
    import lib.*
    val x1 = MyIArray("a")
    val x2 = MyIArray(x1)
    val x3: MyIArray[MyIArray[String]] = x2
  }
  def test2b: Unit = {
    import lib2.*
    val x1 = MyIArray("a")
    val x2 = MyIArray(x1)
    val x3: MyIArray[MyIArray[String]] = x2
  }
  def test2c: Unit = {
    import lib3.*
    val x1 = MyIArray("a")
    val x2 = x1.apply
    val x3: MyIArray[MyIArray[String]] = x2
  }
  def test3: Unit = {
    val x1 = Array("a")
    val x2 = Array(x1)
    val x3: Array[Array[String]] = x2
  }
}

object lib {
  import reflect.*

  opaque type MyIArray[+T] = Array[? <: T]

  object MyIArray {
    extension [T](arr: MyIArray[T]) {
      def apply(n: Int): T = arr.asInstanceOf[Array[T]].apply(n)
    }
    def apply[T](xs: T*)(using ClassTag[T]): MyIArray[T] = Array(xs*)
  }
}
object lib2 {
  import reflect.*

  opaque type MyIArray[+T] = Array[? <: T]

  object MyIArray {
    extension [T](arr: MyIArray[T]) {
      def apply(n: Int): T = arr.asInstanceOf[Array[T]].apply(n)
    }
    def apply[T](x: T)(using ClassTag[T]): MyIArray[T] = Array(x)
  }
}
object lib3 {
  import reflect.*

  opaque type MyIArray[+T] = Array[? <: T]

  object MyIArray {
    extension [T](arr: MyIArray[T]) {
      def apply(n: Int): T = arr.asInstanceOf[Array[T]].apply(n)
      def apply(using ClassTag[MyIArray[T]]): MyIArray[MyIArray[T]] = Array(arr)
    }
    def apply[T](xs: T*)(using ClassTag[T]): MyIArray[T] = Array(xs*)
  }
}
