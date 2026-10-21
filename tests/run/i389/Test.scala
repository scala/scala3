// Regression test for scala/scala3#389: the JVM backend honours the `@Retention` of a
// Java-defined annotation. The annotations are read from Java *source* by dotty's Java
// parser (they are passed to dotc, not pre-compiled), which is the path #389 was about.
//
// Reflection can only observe runtime-visible annotations, so it distinguishes RUNTIME
// (present) from CLASS/SOURCE (absent), but not CLASS (emitted invisible) from SOURCE
// (dropped). A Java annotation with no `@Retention` is intentionally not asserted here:
// its visible-vs-invisible default is current behavior, not a frozen contract.

@RuntimeAnn @ClassAnn @SourceAnn
class Annotated:
  @RuntimeAnn @ClassAnn @SourceAnn def method(): Unit = ()

object Test:
  def main(args: Array[String]): Unit =
    val cls = classOf[Annotated]
    assert(cls.isAnnotationPresent(classOf[RuntimeAnn]), "RuntimeAnn should be visible at runtime on class")
    assert(!cls.isAnnotationPresent(classOf[ClassAnn]), "ClassAnn should not be visible at runtime on class")
    assert(!cls.isAnnotationPresent(classOf[SourceAnn]), "SourceAnn should not be visible at runtime on class")

    val m = cls.getDeclaredMethod("method")
    assert(m.isAnnotationPresent(classOf[RuntimeAnn]), "RuntimeAnn should be visible at runtime on method")
    assert(!m.isAnnotationPresent(classOf[ClassAnn]), "ClassAnn should not be visible at runtime on method")
    assert(!m.isAnnotationPresent(classOf[SourceAnn]), "SourceAnn should not be visible at runtime on method")
