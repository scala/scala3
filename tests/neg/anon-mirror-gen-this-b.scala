import scala.deriving.Mirror

@main def Test = {

  object Bar {
    case object A extends Foo.Item
  }

  object Foo {

    sealed trait Item {

      val mItem = summon[Mirror.Of[Item.this.type]] // error: Bar.A.type is not a subtype of thisItem.type

    }
  }

  assert(Bar.A != null)

}
