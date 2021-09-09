package tests.snippetTestcase3

class SnippetTestcase3:
  /**
    * SNIPPET(OUTERLINEOFFSET:8,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:5,INNERCOLUMNOFFSET:2)
    * ERROR(LINE:8,COLUMN:6)
    * ```scala sc:fail
    * @using compiler.setting "-Xfatal-warnings"
    * try { 2 + 2 }
    * ```
    *
    * SNIPPET(OUTERLINEOFFSET:15,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:5,INNERCOLUMNOFFSET:2)
    * WARNING(LINE:15,COLUMN:6)
    * ```scala sc:compile
    * try { 2 + 2 }
    * ```
    *
    */
  def foo: Int = 0
  /**
    * SNIPPET(OUTERLINEOFFSET:23,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:5,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile
    * @using compiler.setting "-Xfatal-warnings", "-language:experimental.fewerBraces"
    * val elems: List[Int]
    * val xs = elems.map x =>
    *   val y = x - 1
    *   y * y
    * xs.foldLeft:
    *     0
    *   :
    *     (x, y) =>
    *       x + y
    * ```
    *
    * SNIPPET(OUTERLINEOFFSET:42,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:5,INNERCOLUMNOFFSET:2)
    * ERROR(LINE:43,COLUMN:27)
    * ERROR(LINE:48,COLUMN:7)
    * ERROR(LINE:43,COLUMN:21)
    * ERROR(LINE:43,COLUMN:27)
    * ERROR(LINE:44,COLUMN:15)
    * ```scala sc:fail
    * val elems: List[Int]
    * val xs = elems.map x =>
    *   val y = x - 1
    *   y * y
    * xs.foldLeft:
    *     0
    *   :
    *     (x, y) =>
    *       x + y
    * ```
    *
    */
  def bar: Int = 0
  /**
    * SNIPPET(OUTERLINEOFFSET:59,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:5,INNERCOLUMNOFFSET:2)
    * WARNING(LINE:59,COLUMN:0)
    * ```scala sc:compile
    * @using compiler.setting "-totally-random-setting"
    * val a = 2 + 2
    * ```
    */
  def bazz: Int = 0
