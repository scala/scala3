
import annotation.targetName

class Gamma {

  def foo: Int = 1

}

class Delta extends Gamma {      // error: name clash
  @targetName("foo") def bar: Int = 1
}