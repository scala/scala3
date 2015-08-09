object Test {
  var implicitsCache = null
  
  def main(args: Array[String]): Unit = {
     implicitsCache = try{null} catch { case ex: Exception => null }
  }
}
