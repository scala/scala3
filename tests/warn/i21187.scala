//> using options -Wall

def oops(msg: String) = sys.error(msg)

class Zone
object Zone:
  inline def apply[T](inline f: Zone ?=> T): T = f(using new Zone)

inline def zone[A](inline f: Zone ?=> A) = Zone.apply(z => f(using z)) // warn suspicious contextualizing

def zone_?[A](f: Zone ?=> A) = Zone.apply(z => f(using z)) // warn

// intended
//inline def zone[A](inline f: Zone ?=> A): A = Zone.apply(z ?=> f(using z))

@main def hello = 
  // this swallows exceptions!
  zone(oops("here")) // warn function value is not used
  zone_?(oops("here")) // warn
  
  // this doesn't
  Zone(oops("not here"))
