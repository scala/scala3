package dotty

/** Jars used when compiling test, defaults to sbt locations */
object Jars {
  val dottyLib: String = sys.env.get("DOTTY_LIB") getOrElse {
    "../library/target/scala-2.11/dotty-library_2.11-0.1-SNAPSHOT.jar"
  }

  val dottyCompiler: String = sys.env.get("DOTTY_COMPILER") getOrElse {
    "./target/scala-2.11/dotty-compiler_2.11-0.1-SNAPSHOT.jar"
  }

  val dottyInterfaces: String = sys.env.get("DOTTY_INTERFACE") getOrElse {
    "../interfaces/target/dotty-interfaces-0.1-SNAPSHOT.jar"
  }

  val dottyExtras: List[String] = sys.env.get("DOTTY_EXTRAS")
    .map(_.split(",").toList).getOrElse(Nil)

  val dottyTestDeps: List[String] =
    dottyLib :: dottyCompiler :: dottyInterfaces :: dottyExtras
}
