import scala.reflect.Selectable.reflectiveSelectable

type Runner = { def run(args: Array[String]): Unit }

def test(runner: Runner): Unit = runner.run(???)
