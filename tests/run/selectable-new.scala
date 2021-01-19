@main def Test =
  val x =
    class C extends reflect.Selectable with
      def name: String = "hello"
    new C

  val y = new reflect.Selectable with
    def name: String = "hello"

  assert(x.name == "hello")
  assert(y.name == "hello")
