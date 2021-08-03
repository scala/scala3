package tests
package snippetTestcase2

trait Quotes2[A] {
  val r1: r1Module[_] = ???
  trait r1Module[A] {
    type X
    object Y {
      /**
        * SNIPPET(OUTERLINEOFFSET:13,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:8,INNERCOLUMNOFFSET:6)
        * ERROR(LINE:13,COLUMN:12)
        * ```scala sc:fail
        * 2 + List()
        * ```
        *
        */
       type YY
     }
     val z: zModule = ???
     trait zModule {
      /**
        * SNIPPET(OUTERLINEOFFSET:25,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:9,INNERCOLUMNOFFSET:6)
        * ERROR(LINE:25,COLUMN:12)
        * ```scala sc:fail
        * 2 + List()
        * ```
        *
        */
       type ZZ
     }
  }
  object r2 {
    type X
    object Y {
      /**
        * SNIPPET(OUTERLINEOFFSET:39,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:7,INNERCOLUMNOFFSET:6)
        * ERROR(LINE:39,COLUMN:12)
        * ```scala sc:fail
        * 2 + List()
        * ```
        *
        */
      type YY
    }
    val z: zModule = ???
    trait zModule {
      /**
        * SNIPPET(OUTERLINEOFFSET:51,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:8,INNERCOLUMNOFFSET:6)
        * ERROR(LINE:51,COLUMN:12)
        * ```scala sc:fail
        * 2 + List()
        * ```
        *
        */
      type ZZ
    }
  }
}