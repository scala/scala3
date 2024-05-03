// won't compile unless classpath is set correctly
import dotty.tools.tasty.TastyFormat

// def main(args: Array[String]) = // MIGRATION: Scala CLI expects `*.sc` files to be straight-line code
  val cp = sys.props("java.class.path")
  printf("unglobbed classpath: %s\n", cp)
