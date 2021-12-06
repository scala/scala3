import util.matching.Regex
import util.matching.Regex.Match

// Demonstrate what used to be a failure in specs2, before we refined
// the scheme when not to typecheck a function argument again.
object Test:

  extension (s: String)

    def replaceAll(pairs: (String, String)*): String =
      pairs.foldLeft(s) { (res, cur) =>
        res.replaceAll(cur._1, cur._2)
      }

    def replaceAll(exp: String, f: String => String): String =
      new Regex(exp).replaceAllIn(s, (m: Match) => f(m.group(0).replace("\\", "\\\\")))

    def replaceInsideTag(tag: String, p: (String, String)*): String =
      s.replaceAll(tag, (s: String) => java.util.regex.Matcher.quoteReplacement(s.replaceAll(p*)))

