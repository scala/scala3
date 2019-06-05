
import annotation.alpha

class Gamma {

  def foo2() = {
    def bar = 1
    @alpha("bar") def baz = 2 // error: @alpha annotation clashes

    @alpha("bam") def x1 = 0
    @alpha("bam") def y1 = 0 // error: @alpha annotation clashes

  }
}
