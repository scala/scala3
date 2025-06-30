enum FileExtension(val toLowerCase: String):
  case Tasty extends FileExtension("tasty")
  case Betasty extends FileExtension("betasty")
  case Class extends FileExtension("class")
  case Jar extends FileExtension("jar")
  case Scala extends FileExtension("scala")
  case ScalaScript extends FileExtension("sc")
  case Java extends FileExtension("java")
  case Zip extends FileExtension("zip")
  case Inc extends FileExtension("inc")
  case Empty extends FileExtension("")

  /** Fallback extension */
  case External(override val toLowerCase: String) extends FileExtension(toLowerCase)

object O:
  val a = FileExtension.Empty