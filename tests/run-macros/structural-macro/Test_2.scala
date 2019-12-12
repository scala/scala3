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

    // val res2: Person = toSelectable[Record](res)

    // new Record((res2._1._1, res2._1._2), (res2._2._1, res2._2._2)).asInstanceOf[Record {val name: String; val age: Int} ]
  }
}
