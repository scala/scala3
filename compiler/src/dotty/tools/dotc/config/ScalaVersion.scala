/* @author  James Iry
 */
package dotty.tools
package dotc.config

import scala.annotation.internal.sharable
import scala.util.{Try, Success, Failure}

/**
 * Represents a single Scala version in a manner that
 * supports easy comparison and sorting.
 */
sealed abstract class ScalaVersion extends Ordered[ScalaVersion] {
  def unparse: String
}

/**
 * A scala version that sorts higher than all actual versions
 */
@sharable case object NoScalaVersion extends ScalaVersion {
  def unparse: String = "none"

  def compare(that: ScalaVersion): Int = that match {
    case NoScalaVersion => 0
    case _ => 1
  }
}

/**
 * A specific Scala version, not one of the magic min/max versions. An SpecificScalaVersion
 * may or may not be a released version - i.e. this same class is used to represent
 * final, release candidate, milestone, and development builds. The build argument is used
 * to segregate builds
 */
case class SpecificScalaVersion(major: Int, minor: Int, rev: Int, build: ScalaBuild) extends ScalaVersion {
  def unparse: String = s"${major}.${minor}.${rev}.${build.unparse}"

  def compare(that: ScalaVersion): Int =  that match {
    case SpecificScalaVersion(thatMajor, thatMinor, thatRev, thatBuild) =>
      // this could be done more cleanly by importing scala.math.Ordering.Implicits, but we have to do these
      // comparisons a lot so I'm using brute force direct style code
      if (major < thatMajor) -1
      else if (major > thatMajor) 1
      else if (minor < thatMinor) -1
      else if (minor > thatMinor) 1
      else if (rev < thatRev) -1
      else if (rev > thatRev) 1
      else build compare thatBuild
    case AnyScalaVersion => 1
    case NoScalaVersion => -1
  }
}

/**
 * A Scala version that sorts lower than all actual versions
 */
@sharable case object AnyScalaVersion extends ScalaVersion {
  def unparse: String = "any"

  def compare(that: ScalaVersion): Int = that match {
    case AnyScalaVersion => 0
    case _ => -1
  }
}

/**
 * Methods for parsing ScalaVersions
 */
@sharable object ScalaVersion {
  private val dot = "\\."
  private val dash = "\\-"
  private def not(s:String) = s"[^${s}]"
  private val R = s"((${not(dot)}*)(${dot}(${not(dot)}*)(${dot}(${not(dash)}*)(${dash}(.*))?)?)?)".r

  def parse(versionString : String): Try[ScalaVersion] = {
    def failure = Failure(new NumberFormatException(
      s"There was a problem parsing ${versionString}. " +
       "Versions should be in the form major[.minor[.revision]] " +
       "where each part is a positive number, as in 2.10.1. " +
       "The minor and revision parts are optional."
    ))

    def toInt(s: String | Null) = s match {
      case null | "" => 0
      case _ => s.nn.toInt
    }

    def isInt(s: String) = Try(toInt(s)).isSuccess

    import ScalaBuild.*

    def toBuild(s: String | Null) = s match {
      case null | "FINAL" => Final
      case s if (s.toUpperCase.startsWith("RC") && isInt(s.substring(2))) => RC(toInt(s.substring(2)))
      case s if (s.toUpperCase.startsWith("M") && isInt(s.substring(1))) => Milestone(toInt(s.substring(1)))
      case _ => Development(s.nn)
    }

    try versionString match {
      case "" | "any" => Success(AnyScalaVersion)
      case "none" => Success(NoScalaVersion)
      case R(_, majorS, _, minorS, _, revS, _, buildS) =>
        Success(SpecificScalaVersion(toInt(majorS), toInt(minorS), toInt(revS), toBuild(buildS)))
      case _ => failure
    }
    catch {
      case e: NumberFormatException => failure
    }
  }

  /**
   * The version of the compiler running now
   */
  val current: ScalaVersion = parse(util.Properties.versionNumberString).get
}

/**
 * Represents the data after the dash in major.minor.rev-build
 */
abstract class ScalaBuild extends Ordered[ScalaBuild] {
  /**
   * Return a version of this build information that can be parsed back into the
   * same ScalaBuild
   */
  def unparse: String
}

object ScalaBuild {

  /** A development, test, nightly, snapshot or other "unofficial" build
   */
  case class Development(id: String) extends ScalaBuild {
    def unparse: String = s"-${id}"

    def compare(that: ScalaBuild): Int = that match {
      // sorting two development builds based on id is reasonably valid for two versions created with the same schema
      // otherwise it's not correct, but since it's impossible to put a total ordering on development build versions
      // this is a pragmatic compromise
      case Development(thatId) => id compare thatId
      // assume a development build is newer than anything else, that's not really true, but good luck
      // mapping development build versions to other build types
      case _ => 1
    }
  }

  /** A final build
   */
  case object Final extends ScalaBuild {
    def unparse: String = ""

    def compare(that: ScalaBuild): Int = that match {
      case Final => 0
      // a final is newer than anything other than a development build or another final
      case Development(_) => -1
      case _ => 1
    }
  }

  /** A candidate for final release
   */
  case class RC(n: Int) extends ScalaBuild {
    def unparse: String = s"-RC${n}"

    def compare(that: ScalaBuild): Int = that match {
      // compare two rcs based on their RC numbers
      case RC(thatN) => n - thatN
      // an rc is older than anything other than a milestone or another rc
      case Milestone(_) => 1
      case _ => -1
    }
  }

  /** An intermediate release
   */
  case class Milestone(n: Int) extends ScalaBuild {
    def unparse: String = s"-M${n}"

    def compare(that: ScalaBuild): Int = that match {
      // compare two milestones based on their milestone numbers
      case Milestone(thatN) => n - thatN
      // a milestone is older than anything other than another milestone
      case _ => -1
    }
  }
}

