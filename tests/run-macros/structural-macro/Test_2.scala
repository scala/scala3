import Macro._

object Test {

  type Person = Record {
    val name: String
    val age: Int
  }

  def main(args: Array[String]): Unit = {
    val person: Person = Record("name" -> "Emma", "age" -> 42).asInstanceOf[Person]

    val res: (("name", String), ("age", Int)) = toHMap(person)

    println(res)
    println()

    val res2: Person = toSelectable(res)

    println(res2)
  }
}
