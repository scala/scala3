package tests
package secondaryconstructors

class Person(val firstName: String, val lastName: String, val age: Int):
  def this(firstName: String) = this(firstName, "", 0)
  def this(firstName: String, lastName: String) = this(firstName, lastName, 0)

