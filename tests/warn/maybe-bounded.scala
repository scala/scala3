//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

// Matches on abstract types bounded by a maybe type are checked
// against the bounding maybe type.

def f1[T <: String?](x: T): String = x match
  case Ok(y) => y
  case null => "none"

def f2[T <: String?](x: T): String = x match   // warn: not exhaustive, would fail on null
  case Ok(y) => y

def f3[T <: String?](x: T): String = x match
  case Ok(y) => y
  case null => "none"
  case _ => "other"                            // ok, but should be warn: unreachable
                                               // same as for Option (see f3a)

def f3a[T <: Option[String]](x: T): String = x match
  case Some(y) => y
  case None => "none"
  case _ => "other"                            // ok, but should be warn: unreachable

def f4[T <: String?](x: T): String = x match
  case Ok(y) => y
  case null => "none"
  case Ok(z) => z                              // warn: unreachable

def g1[T <: (String ? Exception)](x: T): String = x match
  case Ok(y) => y
  case Err(e) => e.getMessage

def g2[T <: (String ? Exception)](x: T): String = x match   // warn: not exhaustive, would fail on Err(_)
  case Ok(y) => y

def g3[T <: (String ? Exception)](x: T): String = x match
  case Ok(y) => y
  case Err(e) => e.getMessage
  case _ => "other"                            // ok, but should be warn: unreachable
