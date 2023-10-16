package example

import reflect.Selectable.reflectiveSelectable

object StructuralTypes:
  type User = {
    def name: String
    def age: Int
    def foo(x: Int): Int
  }

  val user = null.asInstanceOf[User]
  user.name
  user.age
  val fooBar = user foo 123

  val V: Object {
    def scalameta: String
  } = new:
    def scalameta = "4.0"
  V.scalameta
end StructuralTypes