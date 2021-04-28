object B {
  def main(args: Array[String]): Unit = {
    val a = new A

    a.poly1(new Poly(1))
    a.poly2(new Poly(""))
    a.poly3(new Poly(Array(1)))
    a.poly4(new Poly(Array("")))

    a.arr1(new Arr(Array(1)))
    a.arr2(new Arr(Array("")))
    a.arr3(new Arr(Array(Array(1))))
    a.arr4(new Arr(Array(Array(""))))

    a.arrRef1(new ArrRef(Array(1)))
    a.arrRef2(new ArrRef(Array("")))
    a.arrRef3(new ArrRef(Array(Array(1))))
    a.arrRef4(new ArrRef(Array(Array(""))))
  }
}
