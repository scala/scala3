package dotty.tools.dotc.interactive

import dotty.tools.dotc.config.JavaPlatform
import dotty.tools.dotc.classpath.ClassPathFactory
import dotty.tools.io.ClassPath
import java.io.File
import dotty.tools.dotc.config.PathResolver
import dotty.tools.dotc.core.Contexts.Context
import java.nio.file.Paths

class InteractivePlatform(sourcepath: String)(using Context) extends JavaPlatform{

  private var interactiveCurrentClassPath: Option[ClassPath] = None

  val rootPackage: LogicalPackage = new LogicalPackagesProvider(sourcepath).root
  override def classPath(using Context) : ClassPath = {
    if (interactiveCurrentClassPath.isEmpty) {
      val resolver = new InteractivePathResolver(
        rootPackage,
      )
      interactiveCurrentClassPath = Some(resolver.result)
    }
    interactiveCurrentClassPath.get
  }
}


class InteractivePathResolver(rootPackage: LogicalPackage)(using Context) extends PathResolver {

  override protected val classPathFactory = new InteractiveClassPathFactory(rootPackage)

}


/**
 * A ClassPath factory for source elements, based on logical packages.
 */
class InteractiveClassPathFactory(
    rootPackage: LogicalPackage,
) extends ClassPathFactory {

  /**
   * Creators for sub classpaths which preserve this context.
   */
  override def sourcesInPath(path: String)(using Context): List[ClassPath] = {
    List(
      new LogicalSourcePath(
        path.split(File.pathSeparator).map(new File(_)),
        rootPackage
      )
    )
  }
}
