object example {
  trait MyType[A]
  type Alias[A, B] = MyType[B]
}

object bug {
  export example.{MyType, Alias}
  def bug[A](m: MyType[A]): MyType[A] = m
  val bug2: MyType[String] => MyType[String] = m => m
  def bug3[A, B](m: Alias[A, B]): MyType[B] = m
  def bug4[A, B](m: Alias[A, B]): Alias[Int, B] = m

  //it works when referencing the original type in the parameter position.
  def thisWorks[A](m: example.MyType[A]): MyType[A] = m
  val thisWorks2: example.MyType[String] => MyType[String] = m => m
  val thisWorks3: MyType[String] = (??? : MyType[String])
}