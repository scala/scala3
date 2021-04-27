class Poly[A](val value: A) extends AnyVal

class Arr[A](val value: Array[A]) extends AnyVal

class ArrRef[A <: AnyRef](val value: Array[A]) extends AnyVal

class A {
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
}
