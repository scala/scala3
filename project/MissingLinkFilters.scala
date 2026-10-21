package dotty.tools.sbtplugin

object MissingLinkFilters {
  val excludedClassFiles = Set(
    // All these are copied from Scala 2,
    // it should never be reachable unless code has heavily inlined
    // which itself is not binary compatible
    "scala.Enumeration$ValueSet$$anon$1",
    "scala.collection.Iterator$$anon$3$$anon$4$$anon$5",
    "scala.collection.Iterator$$anon$3$$anon$4",
    "scala.collection.LazyZip2$$anon$7$$anon$8",
    "scala.collection.LazyZip3$$anon$15$$anon$16",
    "scala.collection.LazyZip4$$anon$23$$anon$24",
    "scala.collection.immutable.RedBlackTree$partitioner$1$",
    "scala.math.Ordering$$anonfun$orElse$2",
    "scala.math.Ordering$$anonfun$orElseBy$2",
    "scala.util.control.Exception$$anonfun$pfFromExceptions$1",
  )
}
