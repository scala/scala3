trait MyBase extends App

// a class, not an object
class C extends App:
  println("c")

// App mixed with another parent
object D extends App with Serializable:
  println("d")

object E extends Serializable with App:
  println("e")

// only indirectly an App (warned, but not rewritten)
object F extends MyBase:
  println("f")

// body uses App-specific API
object UsesApi extends App:
  println(executionStart)

// member definitions and statements interleaved
object Interleaved extends App:
  println("before")
  val x = 1
  println(x)

// only definitions, no statements to run in main
object OnlyDefs extends App:
  val x = 1

// a preserved member references `args`, which only exists inside `main`
object UsesArgsInMember extends App:
  def first = args.head
  println(first)

// a preserved member overrides inherited App/DelayedInit API
object OverridesApi extends App:
  override def delayedInit(body: => Unit): Unit = body
  println("run")
