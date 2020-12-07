package dotty.dokka.tasty.comments

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class MarkdownConverterTests {
  @Test def test(): Unit = {
    assertEquals(("a", "b c d"), MarkdownConverter.splitWikiLink("a b c d"))
    assertEquals(("a", "b\\ c d"), MarkdownConverter.splitWikiLink("a b\\ c d"))
    assertEquals(("a\\ b", "c d"), MarkdownConverter.splitWikiLink("a\\ b c d"))
    assertEquals(("a\\\\", "b c d"), MarkdownConverter.splitWikiLink("a\\\\ b c d"))
  }
}
