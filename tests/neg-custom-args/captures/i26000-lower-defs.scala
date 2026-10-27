import language.experimental.captureChecking

class File
class Box[A](val value: A)

// A local def with an inferred result type
def viaDef(io: File^) =
  def g = Box([C^ >: {io}] => (y: File^{C}) => y)
  g.value[{}](File()) // error
  val r: File^{io} = g.value[{io}](io)

// The lower bound refers to a parameter of the def
def viaDefParam(io: File^, x: File^) =
  def mk(z: File^) = Box([C^ >: {z}] => (y: File^{C}) => y)
  mk(io).value[{}](File()) // error
  mk(io).value[{x}](File()) // error
  val r: File^{io} = mk(io).value[{io}](io)

// A top-level def (exempt since it is in the empty package)
def topLevel(io: File^) = Box([C^ >: {io}] => (y: File^{C}) => { val z: File^{C} = io; z })
def useTopLevel(io: File^) =
  topLevel(io).value[{}](File()) // error
  val r: File^{io} = topLevel(io).value[{io}](File())
