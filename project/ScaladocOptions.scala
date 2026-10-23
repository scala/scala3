import dotty.tools.sbtplugin.Versions

object ScaladocOptions {
  // The API docs of each stable release are published under its version, so link the docs
  // of a release to those rather than to nightly, which moves on. RCs aren't published there.
  private def stdlibApiUrl: String =
    if (Versions.isRelease && !Versions.baseVersion.contains('-')) s"https://scala-lang.org/api/${Versions.baseVersion}/"
    else "https://nightly.scala-lang.org/api/"

  def scalacOptionsDocSettings(includeExternalMappings: Boolean = true) = {
    val extMap = Seq("-external-mappings:" +
        (if (includeExternalMappings) s".*scala/.*::scaladoc3::$stdlibApiUrl," else "") +
        ".*java/.*::javadoc::https://docs.oracle.com/en/java/javase/17/docs/api/java.base/")
    Seq(
      "-skip-by-regex:.+\\.internal($|\\..+)",
      "-skip-by-regex:.+\\.impl($|\\..+)",
      "-project-logo", "docs/_assets/images/logo.svg",
      "-social-links:" +
        "github::https://github.com/scala/scala3," +
        "discord::https://discord.com/invite/scala," +
        "twitter::https://twitter.com/scala_lang",
      // contains special definitions which are "transplanted" elsewhere
      // and which therefore confuse Scaladoc when accessed from this pkg
      "-skip-by-id:scala.runtime.stdLibPatches",
      // MatchCase is a special type that represents match type cases,
      // Reflect doesn't expect to see it as a standalone definition
      // and therefore it's easier just not to document it
      "-skip-by-id:scala.runtime.MatchCase",
      "-skip-by-id:dotty.tools.tasty",
      "-skip-by-id:dotty.tools.tasty.util",
      "-skip-by-id:dotty.tools.tasty.besteffort",
      "-project-footer", s"Copyright (c) 2002-$currentYear, LAMP/EPFL",
      "-author",
      "-groups",
      "-default-template", "static-site-main"
    ) ++ extMap
  }

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString
}
