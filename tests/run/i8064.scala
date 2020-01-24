
class Foo[A] private(x: Int) { self => def this() = this(0) }

@main def Test = new Foo[String]()
