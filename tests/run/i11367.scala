// scalajs: --skip

trait NoOuter:
  val outerFields = getClass.getDeclaredFields.filter(_.getName.contains("$outer"))
  if outerFields.nonEmpty then println(s"$getClass has outer fields")

class C extends NoOuter:
  def foo =
    class D extends NoOuter:
      class E extends NoOuter
    class F extends NoOuter
    val d = D()
    d.E()
    F()
  class G extends NoOuter

@main def Test =
  val c = C()
  c.foo
  c.G()
