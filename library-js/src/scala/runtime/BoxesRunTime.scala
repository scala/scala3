package scala.runtime

import scala.language.`2.13`

import scala.math.ScalaNumber

/* The declaration of the class is only to make the JVM back-end happy when
 * compiling the scalalib.
 */
final class BoxesRunTime

object BoxesRunTime {
  def boxToBoolean(b: Boolean): java.lang.Boolean =
    b.asInstanceOf[java.lang.Boolean]

  def boxToCharacter(c: Char): java.lang.Character =
    c.asInstanceOf[java.lang.Character]

  def boxToByte(b: Byte): java.lang.Boolean =
    b.asInstanceOf[java.lang.Boolean]

  def boxToShort(s: Short): java.lang.Short =
    s.asInstanceOf[java.lang.Short]

  def boxToInteger(i: Int): java.lang.Integer =
    i.asInstanceOf[java.lang.Integer]

  def boxToLong(l: Long): java.lang.Long =
    l.asInstanceOf[java.lang.Long]

  def boxToFloat(f: Float): java.lang.Float =
    f.asInstanceOf[java.lang.Float]

  def boxToDouble(d: Double): java.lang.Double =
    d.asInstanceOf[java.lang.Double]

  def unboxToBoolean(b: Any): Boolean = b.asInstanceOf[Boolean]

  def unboxToChar(c: Any): Char = c.asInstanceOf[Char]

  def unboxToByte(b: Any): Byte = b.asInstanceOf[Byte]

  def unboxToShort(s: Any): Short = s.asInstanceOf[Short]

  def unboxToInt(i: Any): Int = i.asInstanceOf[Int]

  def unboxToLong(l: Any): Long = l.asInstanceOf[Long]

  def unboxToFloat(f: Any): Float = f.asInstanceOf[Float]

  def unboxToDouble(d: Any): Double = d.asInstanceOf[Double]

  def equals(x: Object, y: Object): Boolean =
    if (scala.scalajs.js.special.strictEquals(x, y)) true
    else equals2(x, y)

  @inline // only called by equals(), not by codegen
  def equals2(x: Object, y: Object): Boolean = {
    x match {
      case xn: java.lang.Number    => equalsNumObject(xn, y)
      case xc: java.lang.Character => equalsCharObject(xc, y)
      case null                    => y eq null
      case _                       => x.equals(y)
    }
  }

  def equalsNumObject(xn: java.lang.Number, y: Object): Boolean = {
    y match {
      case yn: java.lang.Number    => equalsNumNum(xn, yn)
      case yc: java.lang.Character => equalsNumChar(xn, yc)
      case _ =>
        if (xn eq null)
          y eq null
        else
          xn.equals(y)
    }
  }

  def equalsNumNum(xn: java.lang.Number, yn: java.lang.Number): Boolean = {
    (xn: Any) match {
      case xn: Double =>
        (yn: Any) match {
          case yn: Double      => xn == yn
          case yn: Long        => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case xn: Long =>
        (yn: Any) match {
          case yn: Long        => xn == yn
          case yn: Double      => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case null => yn eq null
      case _    => xn.equals(yn)
    }
  }

  def equalsCharObject(xc: java.lang.Character, y: Object): Boolean = {
    y match {
      case yc: java.lang.Character => xc.charValue() == yc.charValue()
      case yn: java.lang.Number    => equalsNumChar(yn, xc)
      case _ =>
        if (xc eq null)
          y eq null
        else
          false // xc.equals(y) must be false here, because y is not a Char
    }
  }

  @inline
  private def equalsNumChar(xn: java.lang.Number, yc: java.lang.Character): Boolean = {
    (xn: Any) match {
      case xn: Double => xn == yc.charValue()
      case xn: Long   => xn == yc.charValue()
      case _ =>
        if (xn eq null) yc eq null
        else xn.equals(yc)
    }
  }

  @inline
  def hashFromLong(n: java.lang.Long): Int =
    Statics.longHash(n.asInstanceOf[Long])

  @inline
  def hashFromDouble(n: java.lang.Double): Int =
    Statics.doubleHash(n.asInstanceOf[Double])

  @inline
  def hashFromFloat(n: java.lang.Float): Int =
    Statics.floatHash(n.asInstanceOf[Float])

  @inline // called only by ScalaRunTime.hash()
  def hashFromNumber(n: java.lang.Number): Int = {
    (n: Any) match {
      case n: Double => Statics.doubleHash(n)
      case n: Long   => Statics.longHash(n)
      case n         => n.hashCode()
    }
  }

  @inline
  def hashFromObject(a: Object): Int =
    Statics.anyHash(a)
}
