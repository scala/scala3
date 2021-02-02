import Macro.useSchema

@main def Test: Unit = {
  println(useSchema[Byte])
  println(useSchema[Int])
  println(useSchema[Option[Int]])
  println(useSchema[Option[Option[Int]]])
  println(useSchema[Map[Int, Int]])
}
