






object Test {
  import scala.collection.concurrent.TrieMap

  class Collision(val idx: Int) {
    override def hashCode = idx % 10
  }

  def main(args: Array[String]): Unit = {
    val tm = TrieMap[Collision, Unit]()
    for (i <- 0 until 1000) tm(new Collision(i)) = ()

    tm.par.foreach(kv => ())
  }
}

