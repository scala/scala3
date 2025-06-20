//> using options -Wunused:all

class C:
  val x = new reflect.Selectable:
    def f = 42
    def g = 27
  val y: Selectable = new reflect.Selectable:
    def f = 42 // warn
    def g = 27 // warn
  val z = new scala.Selectable:
    def f = 42
    def g = 27
    def selectDynamic(name: String): Any = ???
    def applyDynamic(name: String)(args: Any*): Any = ???
