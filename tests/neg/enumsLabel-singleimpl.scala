enum Ordinalled {

  case A

  def ordinal: Int = -1 // error: the ordinal method of enum class Ordinalled can not be defined by the user

}

trait HasOrdinal { def ordinal: Int = 23 }

enum MyEnum extends HasOrdinal { // error: enum class MyEnum can not inherit the concrete ordinal method of trait HasOrdinal
  case Foo
}
