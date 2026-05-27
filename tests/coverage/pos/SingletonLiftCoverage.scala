/** Coverage test: singleton precision preserved when lifting for coverage.
  *
  * Regression test for the GenBCodeOps crash (Found: Int, Required: (9 : Int)).
  * Both examples trigger LiftCoverage to lift Apply args; the fix preserves
  * singleton types so Ycheck passes.
  */
package covtest

object Ops:
  final val PUBLIC = 1
  final val STATIC = 8

object SingletonLiftMinimal:
  // + 0 forces the arg to be an Apply (lifted by coverage), producing Ops.PUBLIC | x$1
  final val PublicStatic = Ops.PUBLIC | (Ops.STATIC + 0)

object SingletonLiftJavaReflect:
  final val PublicStatic =
    java.lang.reflect.Modifier.PUBLIC | java.lang.reflect.Modifier.STATIC
