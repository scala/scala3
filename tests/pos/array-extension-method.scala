import reflect.ClassTag

extension [A](x: A)
  def array[B >: A: ClassTag](len: Int) =
    Array.fill[B](len)(x)
  def array[B >: A: ClassTag](len1: Int, len2: Int) =
    Array.fill[B](len1, len2)(x)

@main def Test() =
  val n: String | Null = null
  println(n.toString)
  println(n.getClass)
  println(Array.fill[String | Null](10)(elem = null))

  class ArrayBuffer[T]:
      private var elems1 = null.array[Object | Null](16)
      private var elems2 = new Array[Object | Null](16)
      private var elems3 = Array.ofDim[Object | Null](16)
      private var elems4 = Array.fill[Object | Null](16)(elem = null)

      private var m1 = 0.0.array(10, 10)
      private var m2 = 0.0.array[Double](10, 10)
      private var m4 = Array.ofDim[Double](10, 10)
      private var m5 = Array.fill[Double](10, 10)(elem = 0)

      private var indices1 = 0.array(10, 10)
      private var indices2 = 0.array[Int](10, 10)
      private var indices4 = Array.ofDim[Int](10, 10)
      private var indices5 = Array.fill[Int](10, 10)(elem = 0)




