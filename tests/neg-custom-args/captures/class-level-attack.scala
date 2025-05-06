import language.experimental.captureChecking
import caps.*

class IO

class Ref[X](init: X):
  var x = init
  def get: X = x
  def put(y: X): Unit = x = y

class C(io: IO^):
  val r: Ref[IO^] = Ref[IO^](io) // error:
    //Type variable X of constructor Ref cannot be instantiated to box IO^ since
    //that type captures the root capability `cap`.
    // where: ^ refers to the universal root capability
  val r2: Ref[IO^] = Ref(io) // error:
    //Error: Ref[IO^{io}] does not conform to Ref[IO^] (since Refs are invariant)
  def set(x: IO^) = r.put(x)

def outer(outerio: IO^) =
  val c = C(outerio)
  def test(innerio: IO^) =
    c.set(innerio)


