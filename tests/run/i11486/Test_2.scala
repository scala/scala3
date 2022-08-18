// scalajs: --skip

import java.lang.reflect.Executable

case class Tested(foo: Int, bar: String, baz: Double):
  def target(abc: Tested, efg: Tested) = ()
  def symbolic(`def`: Int, *** : Int, `unary_!`: Int) = ()

def run(cls: Class[_]) =
  extension(m: Executable) def parameters: List[String] = m.getParameters.toList.map(_.getName)

  val ctorParams = cls.getConstructors.head.parameters
    assert(ctorParams == List("foo", "bar", "baz"))

    val targetParams = cls.getMethods.toList.find(_.getName == "target").get.parameters
    assert(targetParams == List("abc", "efg"))

    val symbolicParams = cls.getMethods.toList.find(_.getName == "symbolic").get.parameters
    assert(symbolicParams == List("def", "$times$times$times", "unary_$bang"))

@main def Test =
  run(classOf[TestedOld])
  run(classOf[Tested])
