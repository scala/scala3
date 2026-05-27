// https://github.com/scala/scala3/issues/25943
class C25943(val a: AnyRef)

trait Probe25943:
  def aField: AnyRef

def runIt(): Boolean =
  val a2 = new Object

  val obj: Probe25943 =
    new Probe25943:
      class S extends C25943(a2)
      val s = new S{}
      def aField = s.a

  obj.aField eq a2

@main def Test =
  assert(runIt(), "captured value should propagate through super-call")
