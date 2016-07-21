object Test {
  def printList(in: List[String]): Unit = in match {
    case Nil => Unit

    case (s: String) :: Nil =>
      println(s)

    case head :: (s: String) :: Nil =>
      printList(head :: Nil)
      for(i <- head){
        print(i)
      }
      println
      println(s)
  }
}
