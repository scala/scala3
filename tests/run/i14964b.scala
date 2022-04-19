@main def Test: Unit =
  println(summon[reflect.ClassTag[Int]]) // classOf[Int]
  println(summon[reflect.ClassTag[Int | Int]]) // classOf[Int]
  println(summon[reflect.ClassTag[Int | 1]]) // classOf[Int]
  println(summon[reflect.ClassTag[Int | Nothing]]) // classOf[Object]
