object ObscureTasty:

  def foo(f: [t] => List[t] ?=> Unit) = ???
  def test1 = foo([t] => (a: List[t]) ?=> ()) // POLYtype => GIVENMETHODType
  def bar(f: [t] => List[t] => Unit) = ???
  def test2 = bar([t] => (a: List[t]) => ()) // POLYtype => METHODType

  class Bar:
    final val bar = "Bar.bar"

  class Foo extends Bar:
    object A:
      def unapply(a: Any): Some[Foo.super.bar.type] = ???

    def foo =
      "" match
        case A(x) => x // SUPERtype
