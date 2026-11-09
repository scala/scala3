package pkg

/**
 * @define v value
 * @define xxx other
 * @define yyy more
 * @define zzz last
 */
class C {
  /** $v, ${v}, $xxx, ${ xxx}, $yyy, ${yyy }, $zzz, ${   zzz }. */
  def m(): Int = 0
}