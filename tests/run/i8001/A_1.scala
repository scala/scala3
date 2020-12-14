trait TC[A] {
  def apply(a: A): Unit
}

class Poly[A](val value: A) extends AnyVal

class Arr[A](val value: Array[A]) extends AnyVal

class ArrRef[A <: AnyRef](val value: Array[A]) extends AnyVal

class A {
  val tc = new TC[Poly[String]] {
    def apply(a: Poly[String]): Unit = ()
  }

  def poly1(x: Poly[Int]): Poly[Int] =
    new Poly(x.value)

  def poly2(x: Poly[String]): Poly[String] =
    new Poly(x.value)

  def poly3(x: Poly[Array[Int]]): Poly[Array[Int]] =
    new Poly(x.value)

  def poly4(x: Poly[Array[String]]): Poly[Array[String]] =
    new Poly(x.value)

  def arr1(x: Arr[Int]): Arr[Int] =
    new Arr(x.value)

  def arr2(x: Arr[String]): Arr[String] =
    new Arr(x.value)

  def arr3(x: Arr[Array[Int]]): Arr[Array[Int]] =
    new Arr(x.value)

  def arr4(x: Arr[Array[String]]): Arr[Array[String]] =
    new Arr(x.value)

  def arrRef1(x: ArrRef[Integer]): ArrRef[Integer] =
    new ArrRef(x.value)

  def arrRef2(x: ArrRef[String]): ArrRef[String] =
    new ArrRef(x.value)

  def arrRef3(x: ArrRef[Array[Int]]): ArrRef[Array[Int]] =
    new ArrRef(x.value)

  def arrRef4(x: ArrRef[Array[String]]): ArrRef[Array[String]] =
    new ArrRef(x.value)


  def test: Unit = {
    tc.apply(new Poly(""))

    poly1(new Poly(1))
    poly2(new Poly(""))
    poly3(new Poly(Array(1)))
    poly4(new Poly(Array("")))

    arr1(new Arr(Array(1)))
    arr2(new Arr(Array("")))
    arr3(new Arr(Array(Array(1))))
    arr4(new Arr(Array(Array(""))))

    arrRef1(new ArrRef(Array(1)))
    arrRef2(new ArrRef(Array("")))
    arrRef3(new ArrRef(Array(Array(1))))
    arrRef4(new ArrRef(Array(Array(""))))
  }
}
