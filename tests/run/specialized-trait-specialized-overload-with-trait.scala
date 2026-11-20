//>using options -language:experimental.specializedTraits

inline trait PartialValue[T: Specialized](val x: T):
  def add[U](y: U): Long = x.## + y.## 
  inline def add[U: Specialized](y: U): Int = x.## + y.## + 1
  
@main def Test = 
  val x = 42
  val p = new PartialValue(x) {}
  assert(p.add(0) == x + 1)
