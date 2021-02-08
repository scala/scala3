


import collection.*



object Test {

  class AlarmingBuffer[T] extends mutable.ArrayBuffer[T] {
    override def sizeHint(x: Int): Unit = {
      println("Received a size hint: " + x)
      super.sizeHint(x)
    }
  }

  def main(args: Array[String]): Unit = {
    val iteratorBuilder = (new AlarmingBuffer[Int]) mapResult {
      res => res.iterator
    }

    iteratorBuilder.sizeHint(10)
    iteratorBuilder ++= (0 until 10)
    iteratorBuilder.result().foreach(println)
  }

}
