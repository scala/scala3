import scala.language.implicitConversions

final class Functoid[+R](val function: Product => R)

object Functoid {
  implicit def apply[A, R](function: A => R): Functoid[R] = {
    println(s"arity 1")
    new Functoid({ case Tuple1(a: A @unchecked) => function(a) })
  }
  implicit def apply[A, B, R](function: (A, B) => R): Functoid[R] = {
    println("arity 2")
    new Functoid({ case (a: A @unchecked, b: B @unchecked) => function(a, b) })
  }
}

final case class ContainerConfig(image: String, version: Int, cmd: String)

final class ContainerResource

object ContainerResource {
  implicit final class DockerProviderExtensions(private val self: Functoid[ContainerResource]) extends AnyVal {
    def modifyConfig(modify: Functoid[ContainerConfig => ContainerConfig]): Functoid[ContainerConfig => ContainerConfig] = modify
    // removing this overload fixes the implicit conversion and returns `arity 2` print
    def modifyConfig(modify: ContainerConfig => ContainerConfig): Functoid[ContainerConfig => ContainerConfig] = new Functoid(_ => modify)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val cfg = new Functoid(_ => new ContainerResource)
      .modifyConfig {
        // applying Functoid.apply explicitly instead of via implicit conversion also avoids untupling
//        Functoid {
        (image: String, version: Int) => (cfg: ContainerConfig) => cfg.copy(image, version)
//        }
      }
      .function.apply(Tuple2("img", 9))
      .apply(ContainerConfig("a", 0, "b"))
    println(cfg)
  }
}
