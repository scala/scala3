trait T:
  def item: String

object X extends T // error via refchecks

enum Foo {
  case Empty // error via refchecks
  case NonEmpty(item: String)
  case Decoy // missing error

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
