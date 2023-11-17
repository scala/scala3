class C:
  this: C^ =>

  def x: C^{cap[d]} = ??? // error

  def y: C^{cap[C]} = ???  // error
  private val z = (c0: caps.Cap) => (x: Int) => (c: C^{cap[C]}) => x // ok

  private val z2 = identity((x: Int) => (c: C^{cap[z2]}) => x) // error
