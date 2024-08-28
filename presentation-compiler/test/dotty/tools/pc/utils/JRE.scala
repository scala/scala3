package dotty.tools.pc.utils

object JRE:

  def getJavaMajorVersion: Int =
    val javaVersion = sys.env.get("java.version").filter(!_.isEmpty())

    javaVersion match
      case Some(version) if version.startsWith("1.8") => 8
      case _ =>
        scala.util.Try:
          val versionMethod = classOf[Runtime].getMethod("version")
          versionMethod.nn.setAccessible(true)
          val version = versionMethod.nn.invoke(null)

          val majorMethod = version.getClass().getMethod("feature")
          majorMethod.nn.setAccessible(true)
          val major = majorMethod.nn.invoke(version).asInstanceOf[Int]
          major
        .getOrElse(8) // Minimal version supported by Scala


