object a {
  object home { def /(s: String) = createDir }
  object createDir {
    def createDirectoryIfNotExists(createParents: Boolean = false)(using attributes: Seq[String] = Seq.empty, linkOptions: Seq[String] = Seq.empty): Unit =
      println("done")
  }

  //this line causes the crash
  @main def Test = (home/"temp").createDirectoryIfNotExists()
}