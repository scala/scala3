import scala.language.dynamics

object Foo extends Dynamic:
  def applyDynamic(name: String)(): Foo.type = this
  def applyDynamicNamed(name: String)(args: (String, Int)): Foo.type = this
  def selectDynamic(name: String): Foo.type = this

object Test:
  def test(): Foo.type =
    Foo.xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
      .xxx()
      .xxx(a = 1)
      .xxx
