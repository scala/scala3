class Base

object Test {
// OK
val widen = {
  class Inner extends Base
  val f: () => Inner = { () => new Inner }
  f()
}

// Crash
val leak = {
  class Inner extends Base
  val f: (() => Inner){ def apply(): Inner } = { () => new Inner }
  f()
}

}
