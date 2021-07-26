@main def Test =
  val `ct_[[I` = reflect.classTag[Array[Array[Int]] | Array[Array[Int]]]
  val arrArrInt = Array(Array(1))
  assert(`ct_[[I`.runtimeClass == arrArrInt.getClass)
