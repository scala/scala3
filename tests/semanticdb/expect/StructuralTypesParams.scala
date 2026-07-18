package example

import reflect.Selectable.reflectiveSelectable

object StructuralTypesParams:
  type User = {
    def foo(arg1: Int, arg2: String): String
    def age: Int

  }
  val user = null.asInstanceOf[User]
  val num = 123
  val str = "abc"
  val fooBar = user.foo(num, str)
  val fooBaz = user.foo(arg1 = num, arg2 = str)
  val age = user.age

end StructuralTypesParams