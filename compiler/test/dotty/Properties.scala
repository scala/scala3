package dotty

/** Runtime properties from defines or environmnent */
object Properties {

  /** If property is unset or "TRUE" we consider it `true` */
  private[this] def propIsNullOrTrue(prop: String): Boolean = {
    val prop = System.getProperty("dotty.tests.interactive")
    prop == null || prop == "TRUE"
  }

  /** Are we running on the Drone CI? */
  val isRunByDrone: Boolean = sys.env.isDefinedAt("DRONE")

  /** Tests should run interactive? */
  val testsInteractive: Boolean = propIsNullOrTrue("dotty.tests.interactive")

  /** Filter out tests not matching the regex supplied by "dotty.tests.filter"
   *  define
   */
  val testsFilter: Option[String] = sys.props.get("dotty.tests.filter")

  /** Should Unit tests run in safe mode?
   *
   *  For run tests this means that we respawn child JVM processes after each
   *  test, so that they are never reused.
   */
  val testsSafeMode: Boolean = sys.props.isDefinedAt("dotty.tests.safemode")

  /** Dotty compiler path provided through define */
  def dottyCompiler: String = sys.props("dotty.tests.classes.compiler")

  /** Dotty classpath extras provided through define */
  def dottyExtras: List[String] =
    Option(sys.props("dotty.tests.extraclasspath"))
      .map(_.split(":").toList)
      .getOrElse(Nil)

  /** Dotty interfaces path provided through define */
  def dottyInterfaces: String = sys.props("dotty.tests.classes.interfaces")

  /** Dotty library path provided through define */
  def dottyLib: String = sys.props("dotty.tests.classes.library")
}
