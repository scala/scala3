trait T:
  def item: String
object X extends T // refchecks error status quo

enum Foo { // error
  case Empty // refchecks error
  case NonEmpty(item: String)
  case Decoy // hopefully not here

  def item: String
}
