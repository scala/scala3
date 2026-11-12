trait Behavior[A]

object Behavior:
  //implicit def mapBehavior[A: Behavior]: Behavior[List[A]] = new Behavior[List[A]] {}
  given [A: Behavior] => Behavior[List[A]]

object Model:
  case class Foo(i: Int)
  object Foo:
    given Behavior[Foo] = new Behavior[Foo] {}
    //implicit def dummy[A: Behavior](using s: String = "hello"): Option[A] = None
    //implicit def dummy[A: Behavior](using s: String = "hello")(using DummyImplicit): Option[A] = None
    given [A: Behavior] => (s: String = "hello") => Option[A] = None

@main def Test =
  import Model.Foo
  //implicitly[Option[List[Foo]]]
  //Foo.dummy[List[Foo]](using s = "bye")
  summon[Option[List[Foo]]]
