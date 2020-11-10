
import annotation.targetName

class Gamma {

  def foo2() = {
    def bar = 1
    @targetName("bar") def baz = 2 // error: @targetName annotation clashes

    @targetName("bam") def x1 = 0
    @targetName("bam") def y1 = 0 // error: @targetName annotation clashes

  }
}
