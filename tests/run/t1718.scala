object Test extends dotty.runtime.LegacyApp{
  def matchesNull[T](mightBeNull: Array[T]|Null): Boolean = mightBeNull match {
    case null => true
    case x => false
  }

  val nullArray: Array[String]|Null = null
  println(matchesNull(nullArray))
}

