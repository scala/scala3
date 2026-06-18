trait MyTrait[S]:
  val foo: scala.collection.mutable.Map[S, String] = scala.collection.mutable.Map.empty

abstract class MyAbstract[S] extends MyTrait[S]

object Test:
  def main(args: Array[String]): Unit =
    classOf[MyAbstract[Any]].getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
