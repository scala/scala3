object LibraryProperties {
  private val shellBanner: String =
    """%n      ________ ___   / /  ___
       |%n    / __/ __// _ | / /  / _ |
       |%n  __\\ \\/ /__/ __ |/ /__/ __ |
       |%n /____/\\___/_/ |_/____/_/ | |
       |%n                          |/  %s""".stripMargin.replace("\n", "")

  def content(version: String, currentYear: String): String =
    s"""version.number=$version
        |maven.version.number=$version
        |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
        |shell.banner=$shellBanner
        |""".stripMargin
}
