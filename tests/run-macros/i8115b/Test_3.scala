@main def Test() = {
  val typeclass = example.MyClassMaker.make
  assert(typeclass.toString == "MyClassMaker.make.MyClass")
}
