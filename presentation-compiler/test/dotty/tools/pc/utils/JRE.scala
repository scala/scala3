package dotty.tools.pc.utils

object JRE:

  def getJavaMajorVersion: Int =
    val javaVersion = sys.env.get("java.specification.version").filter(!_.isEmpty())

    javaVersion match
      case Some(version) if version.startsWith("1.8") => 8
      case Some(version) => version.toInt // it is better to crash during tests than to run incorrect suite
      case None => 8
