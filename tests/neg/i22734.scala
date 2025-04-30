trait T:
  def item: String
object X extends T // error status quo

enum Foo {
  case Empty // error
  case NonEmpty(item: String)
  case Decoy // hopefully not here

  def item: String
}
