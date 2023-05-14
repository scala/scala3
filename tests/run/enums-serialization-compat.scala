// scalajs: --skip

import java.io.*
import scala.util.Using

enum JColor extends java.lang.Enum[JColor]:
  case Red // java enum has magic JVM support

enum SColor:
  case Green // simple case last

enum SColorTagged[T]:
  case Blue                                     extends SColorTagged[Unit]
  case Rgb(r: Byte, g: Byte, b: Byte)           extends SColorTagged[(Byte, Byte, Byte)] // mixing pattern kinds
  case Indigo                                   extends SColorTagged[Unit]
  case Cmyk(c: Byte, m: Byte, y: Byte, k: Byte) extends SColorTagged[(Byte, Byte, Byte, Byte)] // class case last

enum Nucleobase:
  case A,C,G,T // patdef last

enum MyClassTag[T](wrapped: Class[?]):
  case IntTag  extends MyClassTag[Int](classOf[Int])
  case UnitTag extends MyClassTag[Unit](classOf[Unit]) // value case last

extension (ref: AnyRef) def aliases(compare: AnyRef) = assert(ref eq compare, compare)

@main def Test = Using.Manager({ use =>
  val buf = use(ByteArrayOutputStream())
  val out = use(ObjectOutputStream(buf))
  Seq(JColor.Red, SColor.Green, SColorTagged.Blue, SColorTagged.Indigo).foreach(out.writeObject)
  Seq(Nucleobase.A, Nucleobase.C, Nucleobase.G, Nucleobase.T).foreach(out.writeObject)
  Seq(MyClassTag.IntTag, MyClassTag.UnitTag).foreach(out.writeObject)
  val read = use(ByteArrayInputStream(buf.toByteArray))
  val in   = use(ObjectInputStream(read))

  val Seq(Red @ _, Green @ _, Blue @ _, Indigo @ _) = (1 to 4).map(_ => in.readObject)
  Red    aliases JColor.Red
  Green  aliases SColor.Green
  Blue   aliases SColorTagged.Blue
  Indigo aliases SColorTagged.Indigo

  val Seq(A @ _, C @ _, G @ _, T @ _) = (1 to 4).map(_ => in.readObject)
  A aliases Nucleobase.A
  C aliases Nucleobase.C
  G aliases Nucleobase.G
  T aliases Nucleobase.T

  val Seq(IntTag @ _, UnitTag @ _) = (1 to 2).map(_ => in.readObject)
  IntTag  aliases MyClassTag.IntTag
  UnitTag aliases MyClassTag.UnitTag

}).get
