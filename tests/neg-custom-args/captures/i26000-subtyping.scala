class File

// Polymorphic functions are not subtypes of types with wider bounds
def dropUpper(a: File^, b: File^) =
  val pf = [C^ <: {a}] => (f: File^{C}) => f
  val l1 = pf
  l1[{b}](b) // error
  val l2: [C^] -> (f: File^{C}) -> File^{f} = pf // error

def dropLower(io: File^) =
  val f = [C^ >: {io}] => (y: File^{io}) => (y: File^{C})
  val g: [C^] -> (y: File^{io}) -> File^{C} = f // error

def dropLowerOrdinary(io: File^) =
  val p: [T >: File] => (x: T) => T =
    [T >: File^{io}] => (x: T) => (io: T) // error
