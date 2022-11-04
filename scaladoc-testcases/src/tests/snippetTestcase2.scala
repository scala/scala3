package tests
package snippetTestcase2

trait Quotes2[A] {
  val r1: r1Module[_] = ???
  trait r1Module[A] {
    type X
    object Y {
      /**
        * SNIPPET(OUTERLINEOFFSET:12,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
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
        * SNIPPET(OUTERLINEOFFSET:24,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
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
        * SNIPPET(OUTERLINEOFFSET:38,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
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
        * SNIPPET(OUTERLINEOFFSET:50,OUTERCOLUMNOFFSET:10,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
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
