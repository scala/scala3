package examples.select


import scala.language.dynamics
import examples.select.Wrapper

object Dynamic0:

  @main
  def runTimeError =

    val w = Wrapper(Option(1))
    println(w.isEmpty)

end Dynamic0
