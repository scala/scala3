import scala.quoted.*

package jam {
  trait JamCoreDsl {
    implicit inline def defaultJamConfig: this.JamConfig =
      new JamConfig(brewRecRegex = ".*")
    class JamConfig(val brewRecRegex: String)
    inline def brew(implicit inline config: JamConfig): Unit = ???
  }
  private object internal extends JamCoreDsl
  export internal._
}

object test {
  jam.brew
}
