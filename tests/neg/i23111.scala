trait A:
  def bar: (a: Int, b: Int) => A.this.type = x => ??? // error
  def baz: (a: Int, b: Int) => this.type = x => ???   // error
