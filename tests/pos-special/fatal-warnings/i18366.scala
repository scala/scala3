// scalac: -Wunused:all

trait Builder {
  def foo(): Unit
}

def repro =
  val builder: Builder = ???
  import builder.{foo => bar}
  bar()