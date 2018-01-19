object test {
  class annot extends scala.annotation.Annotation
  def foo = {
    def bar(i: Int): Int = i
    @annot class Silly {} // error: not found
    bar(5)
  }
}
