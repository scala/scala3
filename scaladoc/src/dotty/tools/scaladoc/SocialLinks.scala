package dotty.tools.scaladoc

import java.nio.file.Path
import java.nio.file.Paths
import dotty.tools.dotc.core.Contexts.Context

enum SocialLinks(val url: String, val className: String):
  case Github(ghUrl: String) extends SocialLinks(ghUrl, "gh")
  case Twitter(tUrl: String) extends SocialLinks(tUrl, "twitter")
  case Gitter(gUrl: String) extends SocialLinks(gUrl, "gitter")
  case Discord(dUrl: String) extends SocialLinks(dUrl, "discord")

object SocialLinks:
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
      case _ => Left(errorPrefix)
    }
