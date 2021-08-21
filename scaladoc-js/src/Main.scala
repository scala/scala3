package dotty.tools.scaladoc

object Main:

  private def common(): Unit =
    CodeSnippets()

  def main(): Unit =
    Searchbar()
    SocialLinks()
    DropdownHandler()
    Ux()
    common()

  /**
   * This main is conditionally enabled by system env variable `scaladoc.projectFormat=md`
   * passed in ./projects/scripts/genDocsScalaLang
   * The reason why we have to pass the condition by env variable is because js is build before scaladoc,
   * so we cannot access its args
   */
  def markdownMain(): Unit =
    common()
