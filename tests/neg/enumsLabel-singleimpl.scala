enum Ordinalled {

  case A // error: method ordinal of type => Int needs `override` modifier

  def ordinal: Int = -1

}

trait HasOrdinal { def ordinal: Int = 23 }

enum MyEnum extends HasOrdinal {
  case Foo // error: method ordinal of type => Int needs `override` modifier
}
