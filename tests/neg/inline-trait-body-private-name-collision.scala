inline trait A:
  private val x: Int = 1 // error
  def eq(o: A) = o.x == x
