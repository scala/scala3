package scala3build

import scala.annotation.tailrec

object SjsUnitConstantHolderGenerator {
  /** Generate a *.scala file that contains the given values as literals. */
  def generate(dir: os.Path, fqn: String, values: (String, Any)*): Seq[os.Path] = {
    val (fullPkg@(_ :+ pkg)) :+ objectName = fqn.split('.').toSeq.runtimeChecked

    val out = dir / (objectName + ".scala")

    val defs = for {
      (name, value) <- values
    } yield {
      s"val $name = ${literal(value)}"
    }

    val scalaCode =
      s"""
      package ${fullPkg.mkString(".")}

      private[$pkg] object $objectName {
        ${defs.mkString("\n")}
      }
      """

    os.write(out, scalaCode)

    Seq(out)
  }

  @tailrec
  private final def literal(v: Any): String = v match {
    case s: String  => "raw\"\"\"" + s + "\"\"\""
    case b: Boolean => b.toString
    case i: Int     => i.toString
    case f: os.Path => literal(f.toString)

    case _ =>
      throw new IllegalArgumentException(
          "Unsupported value type: " + v.getClass)
  }
}
