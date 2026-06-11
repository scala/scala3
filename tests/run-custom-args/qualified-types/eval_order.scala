// Skolem ANF lifting must preserve left-to-right evaluation order of the
// effectful arguments of a dependent function call.

def id(x: Int): {r: Int with r == x} = x.runtimeChecked
def add(x: Int, y: Int): {r: Int with r == x + y} = (x + y).runtimeChecked

val log = collection.mutable.ListBuffer.empty[String]
def eff(tag: String, v: Int): Int = { log += tag; v }

@main def Test =
  val c = add(eff("a", 1), id(eff("b", 2)))
  println(c)
  println(log.mkString(","))
