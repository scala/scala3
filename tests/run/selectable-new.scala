@main def Test =
  val x =
    class C extends reflect.Selectable:
      def name: String = "hello"
    new C

  val y = new reflect.Selectable:
    def name: String = "hello"

  assert(x.name == "hello")
  assert(y.name == "hello")
