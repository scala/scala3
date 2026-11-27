import language.experimental.separationChecking
import caps.*

class Box extends Mutable:
  var x = 0
  update def inc(): Unit = x += 1

object ExParam:
  def unapply(consume b: Box^): Option[Int] = None // error

object ExSeqParam:
  def unapplySeq(consume b: Box^): Option[Seq[Int]] = None // error

object ExTrailingUsing:
  def unapply(x: Int)(using consume b: Box^): Option[Int] = None // error

object ExLeadingUsing:
  def unapply(using consume b: Box^)(x: Int): Option[Int] = None // error

class Matcher(b: Box^):
  consume def unapply(x: Int): Option[Int] = None // error

class MatcherSeq(b: Box^):
  consume def unapplySeq(x: Int): Option[Seq[Int]] = None // error

def localExtractor(): Unit =
  def unapply(consume b: Box^): Option[Int] = None // error
  ()

case class ManualCase(consume val b: Box^)
object ManualCase:
  def unapply(consume c: ManualCase^): Option[Box^] = None // error
