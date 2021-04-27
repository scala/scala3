object Test:

  def test1: IArray[Int] = IArray(1, 2) +++ IArray(2, 3)
  def test2: IArray[Int] = IArray(1, 2) +++ List(2, 3)
  def test3 = +++[Int](IArray(1, 2))(IArray(2, 3))
  def test4 = +++[Int](IArray(1, 2))(List(2, 3))
  def test5: IArray[Int] = IArray(1, 2).+++[Int](IArray(2, 3))
  def test6: IArray[Int] = IArray(1, 2).+++[Int](List(2, 3))
  def test7 = +++(IArray(1, 2))[Int](IArray(2, 3))
  def test8 = +++(IArray(1, 2))[Int](List(2, 3))

  extension [A: reflect.ClassTag](arr: IArray[A])
    def +++[B >: A: reflect.ClassTag](suffix: IArray[B]): IArray[B] = ???
    def +++[B >: A: reflect.ClassTag](suffix: IterableOnce[B]): IArray[B] = ???
