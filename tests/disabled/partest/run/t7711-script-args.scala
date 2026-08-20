
import scala.tools.partest.ScriptTest

object Test extends ScriptTest {
  override def extraSettings = s"${super.extraSettings} -Wshadow"
  override def argv          = Seq("good", "news")
}
