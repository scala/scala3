package dotty.dokka.tasty.comments

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class DocFlexmarkParserTests {
  @Test def test(): Unit = {
    assertEquals(("a", "b c d"), DocFlexmarkParser.splitWikiLink("a b c d"))
    assertEquals(("a", "b\\ c d"), DocFlexmarkParser.splitWikiLink("a b\\ c d"))
    assertEquals(("a\\ b", "c d"), DocFlexmarkParser.splitWikiLink("a\\ b c d"))
    assertEquals(("a\\\\", "b c d"), DocFlexmarkParser.splitWikiLink("a\\\\ b c d"))
  }
}
