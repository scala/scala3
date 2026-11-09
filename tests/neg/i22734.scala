trait T:
  def item: String

object X extends T // error via refchecks

enum Foo {
  case Empty // error via refchecks
  case NonEmpty(item: String)
  case NonItem(other: String) // error
  case Extender extends Foo, TCell
  case AlsoT extends Foo, T // error missing error
  case Another extends Foo // error missing error
  case Decoy // error missing error
  case NotSimple(item: String)

  def item: String
}

trait TCell extends T:
  override def item: String = "tcell"

enum Bar:
  case Baz extends Bar, TCell // simple case inherits definition
  def item: String

@main def test =
  println:
    Bar.Baz.item
