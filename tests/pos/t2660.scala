package hoho

class G

class H extends G

class A[T](x: T) {

  def this(y: G, z: T) = {
    this(z)
    print(1)
  }

  def this(z: H, h: T) = {
    this(h)
    print(2)
  }
}

object T {
  def main(args: Array[String]): Unit = {
    implicit def g2h(g: G): H = new H
    new A[Int](new H, 23)
  }
}


// A version of t2660 which does not use constructors

object X {
  def f[T](x: T) = ???
  def f[T](y: G, z: T) = ???
  def f[T](z: H, h: T) = ???
}

object T2 {
  def main(args: Array[String]): Unit = {
    implicit def g2h(g: G): H = new H
    X.f(new H, 23)
  }
}


