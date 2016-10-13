object Test {
  val v: TagAnnotation = null
  println(v.value) // error: value value in class TagAnnotation cannot be accessed as a
                   // member of TagAnnotation(Test.v) from module class Test$.
}
