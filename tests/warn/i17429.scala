case class A(a:Int):
  def plus(a:A) = A(this.a+a.a)
    println(A(1) plus A(2)) // warn
