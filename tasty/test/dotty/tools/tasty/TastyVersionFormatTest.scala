package dotty.tools.tasty

import org.junit.Assert._
import org.junit.{Test, Ignore}

import TastyFormat._
import TastyBuffer._

class TastyVersionFormatTest {

  import TastyVersionFormatTest._

  /** aliases `TastyVersion.apply` */
  def compiler(major: Int, minor: Int, experimental: Int) = TastyVersion(major, minor, experimental)

  /** aliases `TastyVersion.apply` */
  def file(major: Int, minor: Int, experimental: Int) = TastyVersion(major, minor, experimental)

  @Test def accept_ExperimentalReadEQExperimental_EQMinor: Unit = {
    assert(file(28,1,1) <:< compiler(28,1,1)) // same minor, same experimental
  }

  @Test def accept_ExperimentalReadFinal_LTMinor: Unit = {
    assert(file(28,0,0) <:< compiler(28,1,1)) // preceding minor
  }

  @Test def accept_FinalReadFinal_LTEqualMinor: Unit = {
    assert(file(28,0,0) <:< compiler(28,1,0)) // preceding minor
    assert(file(28,0,0) <:< compiler(28,0,0)) // same      minor
  }

  /** these cases are unrelated because a final compiler can only read final tasty of <= minor version */
  @Test def reject_FinalReadFinal_GTMinor: Unit = {
    assert(file(28,2,0) unrelatedTo compiler(28,1,0)) // succeeding minor
  }

  /** these cases are unrelated because a final compiler can not read experimental tasty */
  @Test def reject_FinalReadExperimental: Unit = {
    assert(file(28,0,1) unrelatedTo compiler(28,1,0)) // preceding  minor
    assert(file(28,1,1) unrelatedTo compiler(28,1,0)) // same       minor
    assert(file(28,2,1) unrelatedTo compiler(28,1,0)) // succeeding minor
  }

  /** These cases are unrelated because an experimental compiler can only read final tasty of < minor version */
  @Test def reject_ExperimentalReadFinal_GTEqualMinor: Unit = {
    assert(file(28,2,0) unrelatedTo compiler(28,1,1)) // succeeding minor
    assert(file(28,1,0) unrelatedTo compiler(28,1,1)) // equal      minor
  }

  /**These cases are unrelated because both compiler and file are experimental,
   * and with unequal experimental part.
   */
  @Test def reject_ExperimentalReadNEExperimental: Unit = {
    assert(file(28,1,2) unrelatedTo compiler(28,1,1)) // same minor version, succeeding experimental
    assert(file(28,1,1) unrelatedTo compiler(28,1,2)) // same minor version, preceding  experimental
  }

  /** these cases are unrelated because the major version must be identical */
  @Test def reject_NEMajor: Unit = {
    assert(file(27,0,0) unrelatedTo compiler(28,0,0)) // less    than
    assert(file(29,0,0) unrelatedTo compiler(28,0,0)) // greater than
  }

}

object TastyVersionFormatTest {

  case class TastyVersion(major: Int, minor: Int, experimental: Int) { file =>

    /**if `file <:< compiler` then tasty file is valid to be read.
     *
     * Follows the given algorithm:
     * ```
     * if file.major != compiler.major then
     *   return incompatible
     * if compiler.experimental == 0 then
     *   if file.experimental != 0 then
     *     return incompatible
     *   if file.minor > compiler.minor then
     *     return incompatible
     *   else
     *     return compatible
     * else invariant[compiler.experimental != 0]
     *   if file.experimental == compiler.experimental then
     *     if file.minor == compiler.minor then
     *       return compatible (all fields equal)
     *     else
     *       return incompatible
     *   else if file.experimental == 0,
     *     if file.minor < compiler.minor then
     *       return compatible (an experimental version can read a previous released version)
     *     else
     *       return incompatible (an experimental version cannot read its own minor version or any later version)
     *   else invariant[file.experimental is non-0 and different than compiler.experimental]
     *     return incompatible
     * ```
     */
    def <:<(compiler: TastyVersion): Boolean = (
      file.major == compiler.major && (
        if (file.experimental == compiler.experimental) {
          if (compiler.experimental == 0) {
            file.minor <= compiler.minor
          }
          else {
            file.minor == compiler.minor
          }
        }
        else {
          file.experimental == 0 && file.minor < compiler.minor
        }
      )
    )

    /**if `file unrelated compiler` then tasty file must be rejected.*/
    def unrelatedTo(compiler: TastyVersion): Boolean = !(file <:< compiler)
  }

}
