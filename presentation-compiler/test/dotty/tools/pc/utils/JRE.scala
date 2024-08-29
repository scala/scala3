package dotty.tools.pc.utils

object JRE:

  def getJavaMajorVersion: Int =
    val javaVersion = sys.env.get("java.specification.version").filter(!_.isEmpty())

    javaVersion match
      case Some(version) if version.startsWith("1.8") => 8
      case Some(version) => version
      case None => 8


