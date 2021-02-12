import Macro.*
import Macro2.*

import scala.compiletime.testing.*

object Test {

  type Person = Record {
    val name: String
    val age: Int
  }

  type Person2 = Person

  def main(args: Array[String]): Unit = {
    val person: Person = Record[Person]("name" -> "Emma", "age" -> 42)

    val res = person.toTuple

    val p0 = person.asInstanceOf[Record {
      val name: String
      def age: Int // ignored
    }]
    p0.toTuple

    val p2: Record {
      val age: Int
      val name: String
    } = person

    p2.toTuple : (("age", Int), ("name", String))

    println(res)
    println()

    res: (("name", String), ("age", Int))

    val res2 = Record.fromTuple(res)

    val emptyTuple = Tuple()
    println(Record.fromTuple(emptyTuple))

    val xxl: (("field1", Int),("field2", Int),("field3", Int),("field4", Int),("field5", Int),("field6", Int),("field7", Int),("field8", Int),("field9", Int),("field10", Int),("field11", Int),("field12", Int),("field13", Int),("field14", Int),("field15", Int),("field16", Int),("field17", Int),("field18", Int),("field19", Int),("field20", Int),("field21", Int),("field22", Int),("field23", Int),("field24", Int),("field25", Int)) = ("field1" -> 1,"field2" -> 2,"field3" -> 3,"field4" -> 4,"field5" -> 5,"field6" -> 6,"field7" -> 7,"field8" -> 8,"field9" -> 9,"field10" -> 10,"field11" -> 11,"field12" -> 12,"field13" -> 13,"field14" -> 14,"field15" -> 15,"field16" -> 16,"field17" -> 17,"field18" -> 18,"field19" -> 19,"field20" -> 20,"field21" -> 21,"field22" -> 22,"field23" -> 23,"field24" -> 24,"field25" -> 25)
    println(Record.fromTuple(xxl))

    println(res2)

    res2: Record {
      val name: String
      val age: Int
    }

    res2: Record {
      val age: Int
      val name: String
    }

    val p3: Person2 = person

    p3.toTuple : (("name", String), ("age", Int))

    // Neg-tests
    println(typeCheckErrors("Record.fromTuple((1, 2))").head)

    println(typeCheckErrors("Record.fromTuple((\"field1\" -> 1,\"field2\" -> 2,\"field3\" -> 3,\"field4\" -> 4,\"field5\" -> 5,\"field6\" -> 6,\"field7\" -> 7,\"field8\" -> 8,\"field9\" -> 9,\"field10\" -> 10,\"field11\" -> 11,\"field12\" -> 12,\"field13\" -> 13,\"field14\" -> 14,\"field15\" -> 15,\"field16\" -> 16,\"field17\" -> 17,\"field18\" -> 18,\"field19\" -> 19,\"field20\" -> 20,\"field21\" -> 21,\"field22\" -> 22,\"field23\" -> 23,\"field24\" -> 24,\"field25\" -> 25))").head)

    typeCheckErrors("Record.fromTuple[((\"name\", String), (\"name\", Int))](\"name\" -> \"aa\", \"name\" -> 3)").foreach(println)


  }
}
