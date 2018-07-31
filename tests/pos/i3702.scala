object test {
  class annot extends scala.annotation.Annotation
  def foo = {
    def bar(i: Int): Int = i
    @annot class Silly {} // used to be: not found, but now ok after backing out of 2b12868070be50fb70
    bar(5)
  }
}
