import Macro._

object Test {

  // TODO should elems of `new Record` and `Record.fromUntypedTuple` be IArray[Object]
  // This would make it possible to keep the same reference to the elements when transforming a Tuple into a Record (or vice versa)

  case class Record(elems: (String, Any)*) extends SelectableRecord {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
    override def toString(): String = elems.map(x => x._1 + "=" + x._2).mkString("Record(", ", ", ")")
  }

  object Record extends SelectableRecordCompanion[Record] {
    def fromUntypedTuple(elems: (String, Any)*): Record = Record(elems: _*)
  }

  type Person = Record {
    val name: String
    val age: Int
  }

  def main(args: Array[String]): Unit = {
    val person: Person = Record("name" -> "Emma", "age" -> 42).asInstanceOf[Person]

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

    val emptyTuple = ()
    println(Record.fromTuple(emptyTuple))

    //  println(Record.fromTuple((1, 2))) // error

    val xxl: (("field1", Int),("field2", Int),("field3", Int),("field4", Int),("field5", Int),("field6", Int),("field7", Int),("field8", Int),("field9", Int),("field10", Int),("field11", Int),("field12", Int),("field13", Int),("field14", Int),("field15", Int),("field16", Int),("field17", Int),("field18", Int),("field19", Int),("field20", Int),("field21", Int),("field22", Int),("field23", Int),("field24", Int),("field25", Int)) = ("field1" -> 1,"field2" -> 2,"field3" -> 3,"field4" -> 4,"field5" -> 5,"field6" -> 6,"field7" -> 7,"field8" -> 8,"field9" -> 9,"field10" -> 10,"field11" -> 11,"field12" -> 12,"field13" -> 13,"field14" -> 14,"field15" -> 15,"field16" -> 16,"field17" -> 17,"field18" -> 18,"field19" -> 19,"field20" -> 20,"field21" -> 21,"field22" -> 22,"field23" -> 23,"field24" -> 24,"field25" -> 25)
    println(Record.fromTuple(xxl))
    // println(Record.fromTuple(("field1" -> 1,"field2" -> 2,"field3" -> 3,"field4" -> 4,"field5" -> 5,"field6" -> 6,"field7" -> 7,"field8" -> 8,"field9" -> 9,"field10" -> 10,"field11" -> 11,"field12" -> 12,"field13" -> 13,"field14" -> 14,"field15" -> 15,"field16" -> 16,"field17" -> 17,"field18" -> 18,"field19" -> 19,"field20" -> 20,"field21" -> 21,"field22" -> 22,"field23" -> 23,"field24" -> 24,"field25" -> 25)))

    println(res2)

    // Record.fromTuple[(("name", String), ("name", Int))]("name" -> "aa", "name" -> 3) // error

    res2: Record {
      val name: String
      val age: Int
    }

    res2: Record {
      val age: Int
      val name: String
    }
  }
}
