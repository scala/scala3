package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import collection.JavaConverters._

object Main:
  def main(args: Array[String]): Unit =
    val config = DottyDokkaConfig("Ala.scala")
    new DokkaGenerator(config, DokkaConsoleLogger.INSTANCE).generate()
    println("Done")
