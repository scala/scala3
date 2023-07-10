package dotty.tools.scaladoc

enum SocialLinks(val url: String, val className: String):
  case Github(ghUrl: String) extends SocialLinks(ghUrl, "gh")
  case Twitter(tUrl: String) extends SocialLinks(tUrl, "twitter")
  case Gitter(gUrl: String) extends SocialLinks(gUrl, "gitter")
  case Discord(dUrl: String) extends SocialLinks(dUrl, "discord")
  case Custom(cUrl: String, lightIcon: String, darkIcon: String) extends SocialLinks(cUrl, "custom")

object SocialLinks:
  val LowercaseNamePattern = "^[a-z]+$".r

  def parse(s: String): Either[String, SocialLinks] =
    val errorPrefix = s"Social links arg $s is invalid: "
    val splitted = s.split("::")

    splitted.head match {
      case "github" if splitted.size == 2 => Right(Github(splitted(1)))
      case "github" => Left(errorPrefix + "For 'github' arg expected one argument: url")
      case "twitter" if splitted.size == 2 => Right(Twitter(splitted(1)))
      case "twitter" => Left(errorPrefix + "For 'twitter' arg expected one argument: url")
      case "gitter" if splitted.size == 2 => Right(Gitter(splitted(1)))
      case "gitter" => Left(errorPrefix + "For 'gitter' arg expected one argument: url")
      case "discord" if splitted.size == 2 => Right(Discord(splitted(1)))
      case "discord" => Left(errorPrefix + "For 'discord' arg expected one argument: url")
      case LowercaseNamePattern() if splitted.size == 4 => Right(Custom(splitted(1), splitted(2), splitted(3)))
      case LowercaseNamePattern() if splitted.size == 3 => Right(Custom(splitted(1), splitted(2), splitted(2)))
      case LowercaseNamePattern() => Left(errorPrefix + "For 'custom' two minimum arguments are expected: url, white icon name, [dark icon name]")
      case _ => Left(errorPrefix)
    }
