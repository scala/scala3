class C {
   def bad() = println("I've gone bad!")
}

def newC: {*} C = C()

type Observe[T] = (T => Unit) -> Unit
def unsafe(cap: {*} C) = cap.bad()

def box[T](v: T) : Observe[T] = {
  (fn: T => Unit) => fn(v)
}

def main() : Int = {
   val boxed : Observe[{*} C] = box(newC)
   boxed(unsafe)

   0
}
