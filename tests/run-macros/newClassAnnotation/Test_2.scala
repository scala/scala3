@main def Test =
  val (cls, str) = makeClass("name")
  println(str)
  println(cls.getClass)
  println(cls.getClass.getAnnotation(classOf[JavaAnnot]).value)
