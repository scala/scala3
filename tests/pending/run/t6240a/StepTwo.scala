import java.io.File
import java.net.URLClassLoader

object StepTwo extends dotty.runtime.LegacyApp {
  import scala.reflect.runtime.universe._
  println(typeOf[StepTwo.type])
}
