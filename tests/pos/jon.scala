// This one blows up with a huge type in Scala 2.
// Reported by Jon Pretty in his talk on Scala type inference.
object Test {

  val x = List(List, Vector)

  val y: List[scala.collection.generic.SeqFactory[_]] = x
}
