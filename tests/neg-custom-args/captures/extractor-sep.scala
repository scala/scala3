import language.experimental.separationChecking
import caps.*

class Box extends Mutable:
  var x = 0
  update def inc(): Unit = x += 1

object SepEx:
  def unapply(b1: Box^)(using b2: Box^): Option[Int] =
    b1.inc()
    b2.inc()
    Some(1)

def extractorUsingOverlap(using b: Box^): Unit =
  b match
    case SepEx(_) => () // error
    case _ => ()

class Matcher(b1: Box^):
  def unapply(b2: Box^): Option[Int] =
    b1.inc()
    b2.inc()
    Some(1)

def extractorQualOverlap(b: Box^): Unit =
  val m = Matcher(b)
  b match
    case m(_) => () // error
    case _ => ()

object SepExRo:
  def unapply(b1: Box)(using b2: Box^): Option[Int] =
    b2.inc()
    Some(b1.x)

def extractorRoUsingOverlap(using b: Box^): Unit =
  b match
    case SepExRo(_) => () // error
    case _ => ()

def extractorNonPolyScrutineeOverlap(using b: Box^): Unit =
  object LocalEx:
    def unapply(b1: Box^{b})(using b2: Box^): Option[Int] =
      b1.inc()
      b2.inc()
      Some(1)
  b match
    case LocalEx(_) => () // error
    case _ => ()

def extractorLeadingUsingOverlap(using b: Box^): Unit =
  object LeadEx:
    def unapply(using b2: Box^)(b1: Box^{b}): Option[Int] =
      b1.inc()
      b2.inc()
      Some(1)
  b match
    case LeadEx(_) => () // error
    case _ => ()

// Separate capabilities are allowed
def extractorSeparate(b1: Box^)(using b2: Box^): Unit =
  val m = Matcher(b1)
  b2 match
    case m(_) => ()
    case _ => ()
  b1 match
    case SepEx(_) => ()
    case SepExRo(_) => ()
    case _ => ()
