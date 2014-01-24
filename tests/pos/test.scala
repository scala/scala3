object test {
  import scala.collection.breakOut
 
  def name(i: Int) = i.toString

  (0 until 10).map(name)(breakOut)
}