
import annotation.alpha

class Gamma {

  def foo: Int = 1

}

class Delta extends Gamma {      // error: name clash
  @alpha("foo") def bar: Int = 1
}