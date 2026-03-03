import language.experimental.captureChecking
import caps.*

class IO

class Ref[X](init: X) extends caps.Stateful:
  var x = init
  def get: X = x
  update def put(y: X): Unit = x = y

class C(io: IO^) extends caps.Stateful:
  val r: Ref[IO^]^ = Ref[IO^](io)
    //Type variable X of constructor Ref cannot be instantiated to box IO^ since
    //that type captures the root capability `any`.
    // where: ^ refers to the universal root capability
  val r2: Ref[IO^] = Ref(io)
  update def set(x: IO^) = r.put(x)  // error

def outer(outerio: IO^) =
  val c = C(outerio)
  def test(innerio: IO^) =
    c.set(innerio)


