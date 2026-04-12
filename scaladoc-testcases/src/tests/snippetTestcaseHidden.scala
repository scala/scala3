package tests.snippetTestcaseHidden

class SnippetTestcaseHidden:
  /**
    * ```scala sc-hidden sc-name:preamble
    * val xs: List[Int] = List(1, 2, 3)
    * ```
    *
    * SNIPPET(OUTERLINEOFFSET:11,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile sc-compile-with:preamble
    * val ys = xs.map(x => x * 2)
    * ```
    */
  def a = 3
