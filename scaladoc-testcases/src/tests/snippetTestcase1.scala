package tests.snippetTestcase1

class SnippetTestcase1:
  /**
    * SNIPPET(OUTERLINEOFFSET:8,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:3,INNERCOLUMNOFFSET:2)
    * ERROR(LINE:8,COLUMN:8)
    * ```scala sc:fail
    * 2 + List()
    * ```
    *
    */
  def a = 3
  /**
    * SNIPPET(OUTERLINEOFFSET:16,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:3,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile sc-name:1
    * val xs: List[Int] = List()
    * ```
    *
    * SNIPPET(OUTERLINEOFFSET:21,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:3,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile sc-compile-with:1 sc-name:2
    * val ys = xs.map(x => x * 2)
    * ```
    *
    * SNIPPET(OUTERLINEOFFSET:26,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:3,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile sc-compile-with:2
    * xs ++ ys
    * ```
    */
  def b = 3