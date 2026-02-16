object Def:
  trait A
  trait B
  implicit def conv1(a: A): B = ??? // should not change
  implicit def conv2: A => B = ???
  final implicit def nonConv: A = ???
  implicit val conv3: A => B = ???
  final implicit val nonConv2: A = ???

  implicit class Extension(a: Int): // should not change
    def addedMethod(): A = ???
  implicit class ExtensionWithImplicit(t: String)(implicit a: Int):
    def addedMethod(): String = ???
  class NoNonimplicitParams(implicit a: Int)

  def applicationTest(implicit a: Int): Unit = ???
  val application = applicationTest(0)
  val implicitArg: Int => Unit = (implicit a => applicationTest) // should not change

  implicit def refined(): A {type B = Int} = ???

  class EmptyParamListClass(implicit a: Int)
  def emptyParamListTest() = new EmptyParamListClass()(0)
