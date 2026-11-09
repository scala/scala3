package pkg

/**
 * @define c value
 * @define rec recursive-$c
 * @define escaped escaped-\$c
 */
class C {
  /**
   * Normal: $c, Escaped: \$c, Recursive: $rec, Defined-Escaped: $escaped, Alone: $
   */
  def m(): Int = 0
}