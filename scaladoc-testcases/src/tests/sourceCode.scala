package tests.sourceCode

/**
 * {{{
 * abc {
 *   indentation(2)
 * }
 * }}}
 *
 * {{{
 *  abc {
 *    indentation(2)
 *  }
 * }}}
 */
object indentation:
  /**
   * ```scala
   * abc {
   *   indentation(2)
   * }
   * ```
   *
   * ```
   *  abc {
   *    indentation(2)
   *  }
   * ```
   *
   * @syntax markdown
   */
  def markdown = ???
