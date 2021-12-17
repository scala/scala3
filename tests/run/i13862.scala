trait Foo(val num: Int)                 // a trait with a parameter stored in a val

class Bar(num: Int) extends Foo(num):   // an extending class with a parameter of the same name
  def bar = this.num                    // implicitly creates another num in Bar

@main def Test = Bar(123)

class Bar2(n: Int) extends Foo(n):   // an extending class with a parameter of the same name
  private val num = n
  def bar = this.num                    // implicitly creates another num in Bar

