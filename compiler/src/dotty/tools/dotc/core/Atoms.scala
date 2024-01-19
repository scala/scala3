package dotty.tools
package dotc
package core

import Types.*

/** Indicates the singleton types that a type must or may consist of.
 *  @param lo   The lower bound: singleton types in this set are guaranteed
 *              to be in the carrier type.
 *  @param hi   The upper bound: all singleton types in the carrier type are
 *              guaranteed to be in this set
 *  If the underlying type of a singleton type is another singleton type,
 *  only the latter type ends up in the sets.
 */
enum Atoms:
  case Range(lo: Set[Type], hi: Set[Type])
  case Unknown

  def & (that: Atoms): Atoms = this match
    case Range(lo1, hi1) =>
      that match
        case Range(lo2, hi2) => Range(lo1 & lo2, hi1 & hi2)
        case Unknown => Range(Set.empty, hi1)
    case Unknown =>
      that match
        case Range(lo2, hi2) => Range(Set.empty, hi2)
        case Unknown => Unknown

  def | (that: Atoms): Atoms = this match
    case Range(lo1, hi1) =>
      that match
        case Range(lo2, hi2) => Range(lo1 | lo2, hi1 | hi2)
        case Unknown => Unknown
    case Unknown => Unknown

end Atoms
