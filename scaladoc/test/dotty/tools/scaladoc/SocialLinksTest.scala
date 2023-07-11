package dotty.tools.scaladoc

import org.junit.Test
import org.junit.Assert._
import dotty.tools.scaladoc.SocialLinks

class SocialLinksTest:

  @Test def githubLink(): Unit =
    val githubLink = "github::https://github.com/test"
    val expected = SocialLinks.Github("https://github.com/test")
    assertEquals(expected, SocialLinks.parse(githubLink).getOrElse(null))

  @Test def twitterLink(): Unit =
    val twitterLink = "twitter::https://twitter.com/test"
    val expected = SocialLinks.Twitter("https://twitter.com/test")
    assertEquals(expected, SocialLinks.parse(twitterLink).getOrElse(null))

  @Test def gitterLink(): Unit =
    val gitterLink = "gitter::https://gitter.im/test"
    val expected = SocialLinks.Gitter("https://gitter.im/test")
    assertEquals(expected, SocialLinks.parse(gitterLink).getOrElse(null))

  @Test def discordLink(): Unit =
    val discordLink = "discord::https://discord.gg/test"
    val expected = SocialLinks.Discord("https://discord.gg/test")
    assertEquals(expected, SocialLinks.parse(discordLink).getOrElse(null))

  @Test def customLinkLight(): Unit =
    val customLink = "namecustom::https://custom.com/test::custom"
    val expected = SocialLinks.Custom("https://custom.com/test", "custom", "custom")
    assertEquals(expected, SocialLinks.parse(customLink).getOrElse(null))

  @Test def customLinkLightAndDark(): Unit =
    val customLink = "namecustom::https://custom.com/test::custom::custom-dark"
    val expected = SocialLinks.Custom("https://custom.com/test", "custom", "custom-dark")
    assertEquals(expected, SocialLinks.parse(customLink).getOrElse(null))

  @Test def parseRegexError(): Unit =
    val regexErrorLink = "nameCustom::https://custom.com/test::custom::custom-dark::custom"
    val expected = s"Social links arg $regexErrorLink is invalid: "
    assertEquals(expected, SocialLinks.parse(regexErrorLink).left.getOrElse(null))

  @Test def parseLinkWithError(): Unit =
    val errorLink = "namecustom::https://custom.com/test::custom::custom-dark::custom"
    val expected = s"Social links arg $errorLink is invalid: For 'custom' two minimum arguments are expected: url, white icon name, [dark icon name]"
    assertEquals(expected, SocialLinks.parse(errorLink).left.getOrElse(null))
