import java.io._
import scala.util.Using

enum JColor extends java.lang.Enum[JColor]:
  case Red

enum SColor:
  case Green

enum SColorTagged[T]:
  case Blue extends SColorTagged[Unit]

@main def Test = Using.Manager({ use =>
  val buf = use(ByteArrayOutputStream())
  val out = use(ObjectOutputStream(buf))
  Seq(JColor.Red, SColor.Green, SColorTagged.Blue).foreach(out.writeObject)
  val read = use(ByteArrayInputStream(buf.toByteArray))
  val in   = use(ObjectInputStream(read))
  val Seq(Red @ _, Green @ _, Blue @ _) = (1 to 3).map(_ => in.readObject)
  assert(Red eq JColor.Red, JColor.Red)
  assert(Green eq SColor.Green, SColor.Green)
  assert(Blue eq SColorTagged.Blue, SColorTagged.Blue)
}).get
