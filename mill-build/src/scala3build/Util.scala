package scala3build

import scala.jdk.CollectionConverters.*

object Util {
  lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  lazy val isBsp: Boolean =
    Thread.getAllStackTraces.asScala.exists {
      case (t, _) =>
        t.getName == "mill-bsp-evaluator"
    }
}
