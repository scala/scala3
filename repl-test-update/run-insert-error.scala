def replTestDir = "../compiler/test-resources/repl"

def list(dir: String = "."): Vector[String] =
    Option(new java.io.File(dir).list).map(_.toVector).getOrElse(Vector())

def loadLines(fileName: String, enc: String = "UTF-8"): Vector[String] = 
  var result = Vector.empty[String]
  val source = scala.io.Source.fromFile(fileName, enc)
  try result = source.getLines().toVector finally source.close()
  result

def saveString(text: String, fileName: String, enc: String = "UTF-8"): Unit = 
  val f = new java.io.File(fileName)
  val pw = new java.io.PrintWriter(f, enc)
  try pw.write(text) finally pw.close()


@main 
def run_insert_error(): Unit = 
  for f <- list(replTestDir) do
    val fileName = s"$replTestDir/$f"
    print(s"\nprocessing: $fileName")
    val lines = loadLines(fileName)
    var result = Vector[String]() 
    for i <- lines.indices do
      result :+= lines(i)
      if lines(i).startsWith("scala>") && 
         lines.lift(i + 1).map(_.startsWith("1 | ")).getOrElse(false) 
      then
        result :+= "-- Error:"

    val msg = 
     if lines == result then Console.GREEN + " unmodified" + Console.RESET
     else Console.RED + " *** MODIFIED" + Console.RESET
    println(msg)
    if lines != result then 
      println("\nbefore ==========>\n" + lines.mkString("\n"))
      println("\nafter  ==========>\n" + result.mkString("\n"))
      saveString(result.mkString("\n"), fileName)