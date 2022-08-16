package dotty.tools.scaladoc

case class QuickLink(text: String, url: String)

object QuickLink:
  lazy val delimiter = "::"
  def usage: String =
    """List of quick links that is displayed in the header of documentation.
      |The setting accepts list of quick links in format: text::url
      |The first `::` occurence is taken as the delimiter.""".stripMargin
  def parse(s: String): Either[String, QuickLink] =
    s.split(delimiter, 2).toList match
      case text :: url :: Nil => Right(QuickLink(text, url))
      case _ => Left(s"""Invalid syntax of quick link.
                    |$usage""".stripMargin)
