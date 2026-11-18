//> using options -language:experimental.specializedTraits

inline trait Bin[-T: Specialized]:
  var count: Int = 0
  def throwAway(x: T): String = { count += 1; s"threw away $x" }
  def throwAwayLazy(x: => T): String = s"threw away $x lazily"
  def throwAwayAll(xs: T*): Int = xs.length
  def throwAwayWith(f: Int => T): String = s"threw away ${f(1)}"

def useIntBin(bin: Bin[Int]): Unit =
  var evaluated = 0
  
  // Side-effecting argument: evaluated exactly once
  assert(bin.throwAway({ evaluated += 1; 100 }) == "threw away 100")
  assert(evaluated == 1)
  
  // By-name argument: not evaluated before the call
  assert(bin.throwAwayLazy({ evaluated += 1; 1 }) == "threw away 1 lazily")
  assert(evaluated == 2)
  
  // Repeated arguments
  assert(bin.throwAwayAll(1, 2, 3) == 3)
  
  // Argument containing a definition (closure)
  assert(bin.throwAwayWith(i => i + 1) == "threw away 2")
  
  // Assignment to a member
  bin.count = bin.count + 10
  assert(bin.count == 11)

@main def Test =
  useIntBin(new Bin[Any] {}) 
  useIntBin(new Bin[Int] {}) 
