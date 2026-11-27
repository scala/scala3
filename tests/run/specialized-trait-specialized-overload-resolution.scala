//>using options -language:experimental.specializedTraits

  
def spec1[T](x: T): Short = x.##.toShort
  
inline def spec1[T: Specialized](x: T): Int = x.## + 1
  
def spec2[T, U](x: T, y: U): Short = (x.## + y.##).toShort
  
inline def spec2[T: Specialized, U](x: T, y: U): Int = x.## + y.## + 1
  
inline def spec2[T: Specialized, U: Specialized](x: T, y: U): Long = x.## + y.## + 2

@main def Test = 
  val x = 42 
    
  assert(spec1(x) == x + 1)
  assert(spec2(x, 0) == x + 2)
    
  def forwarder[T](x: Int, y: T): Int = spec2(x, y)
  
  assert(forwarder(x, 0) == x + 1)
