// This does not need deep-sub-type. It was placed here to avoid being run from JS tests.
class Context

def foo =
  val ctx: Context = new Context
  given a: Context = ctx

class C:
  private val ctx: Context = new Context
  given Context = ctx
  given C = this

@main def Test =
  val c = new C()
  println(c.getClass.getDeclaredFields.toList)

