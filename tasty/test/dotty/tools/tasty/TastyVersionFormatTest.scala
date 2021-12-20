package dotty.tools.tasty

import org.junit.Assert._
import org.junit.{Test, Ignore}

import dotty.tools.tasty.TastyVersion

import TastyFormat._
import TastyBuffer._

class TastyVersionFormatTest {

  import TastyVersionFormatTest._

  /** aliases `TastyVersion.apply` */
  def compiler(major: Int, minor: Int, experimental: Experimental) = tastyVersion(major, minor, experimental)

  /** aliases `TastyVersion.apply` */
  def file(major: Int, minor: Int, experimental: Experimental) = tastyVersion(major, minor, experimental)

  @Test def accept_ExperimentalReadEQExperimental_EQMinor: Unit = {
    assert(file(28,1,Exp(1)) <:< compiler(28,1,Exp(1))) // same minor, same experimental
  }

  @Test def accept_ExperimentalReadFinal_LTMinor: Unit = {
    assert(file(28,0,Final) <:< compiler(28,1,Exp(1))) // preceding minor
  }

  @Test def accept_FinalReadFinal_LTEqualMinor: Unit = {
    assert(file(28,0,Final) <:< compiler(28,1,Final)) // preceding minor
    assert(file(28,0,Final) <:< compiler(28,0,Final)) // same      minor
  }

  /** these cases are unrelated because a final compiler can only read final tasty of <= minor version */
  @Test def reject_FinalReadFinal_GTMinor: Unit = {
    assert(file(28,2,Final) unrelatedTo compiler(28,1,Final)) // succeeding minor
  }

  /** these cases are unrelated because a final compiler can not read experimental tasty */
  @Test def reject_FinalReadExperimental: Unit = {
    assert(file(28,0,Exp(1)) unrelatedTo compiler(28,1,Final)) // preceding  minor
    assert(file(28,1,Exp(1)) unrelatedTo compiler(28,1,Final)) // same       minor
    assert(file(28,2,Exp(1)) unrelatedTo compiler(28,1,Final)) // succeeding minor
  }

  /** These cases are unrelated because an experimental compiler can only read final tasty of < minor version */
  @Test def reject_ExperimentalReadFinal_GTEqualMinor: Unit = {
    assert(file(28,2,Final) unrelatedTo compiler(28,1,Exp(1))) // succeeding minor
    assert(file(28,1,Final) unrelatedTo compiler(28,1,Exp(1))) // equal      minor
  }

  /**These cases are unrelated because both compiler and file are experimental,
   * and with unequal experimental part.
   */
  @Test def reject_ExperimentalReadNEExperimental: Unit = {
    assert(file(28,1,Exp(2)) unrelatedTo compiler(28,1,Exp(1))) // same minor version, succeeding experimental
    assert(file(28,1,Exp(1)) unrelatedTo compiler(28,1,Exp(2))) // same minor version, preceding  experimental
  }

  /** these cases are unrelated because the major version must be identical */
  @Test def reject_NEMajor: Unit = {
    assert(file(27,0,Final) unrelatedTo compiler(28,0,Final)) // less    than
    assert(file(29,0,Final) unrelatedTo compiler(28,0,Final)) // greater than
  }

}

object TastyVersionFormatTest {

  type Experimental = Int
  val Final: Experimental = 0
  def Exp(i: Int): Experimental = i.ensuring(_ > 0)

  implicit class TastyVersionOps(fileVersion: TastyVersion) {
    def <:<(compilerVersion: TastyVersion): Boolean = TastyFormat.isVersionCompatible(fileVersion, compilerVersion)

    /**if `file unrelated compiler` then tasty file must be rejected.*/
    def unrelatedTo(compilerVersion: TastyVersion): Boolean = !(fileVersion <:< compilerVersion)
  }

  def tastyVersion(major: Int, minor: Int, experimental: Experimental) = {
    assert(major >= 0)
    assert(minor >= 0)
    assert(experimental >= 0)
    TastyVersion(major, minor, experimental)
  }

}
