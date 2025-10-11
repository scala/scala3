@main def Test: Unit =
  List((42, "asdads")) match
    case List((a, b)) => 1
  List((a = 42, b = "asdads")) match
    case List((a, b)) => 1
  val tuple_list = List((42, "asdads"))
  tuple_list.map((a, b) => println(s"$a $b"))
  val named_tuple_list = List((a = 42, b = "asdads"))
  named_tuple_list.foreach((a, b) => println(s"$a $b"))
  named_tuple_list.foreach { case (a, b) => println(s"$a $b") }
  val l = Seq.empty[(name: String, age: Int)]
  l.map((name, i) => name + i)