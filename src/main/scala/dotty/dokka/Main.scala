package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tastydoc.representations
import dotty.tastydoc.representations._

class DokkaTastyInspector extends TastyInspector:
  private val packages = Set.newBuilder[DDPackage]
  private val classes = Set.newBuilder[DDClass]
  
  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}

    object Traverser extends TreeTraverser:
      override def traverseTree(tree: Tree)(using ctx: Context): Unit = 
        tree match {
          case pc: PackageClause =>
            packages += DDPackage(pc.pid.show, pc.symbol.comment.fold("")(_.raw))
          case clazz: ClassDef  =>
            classes += DDClass(clazz.name, "dotty.dokka", clazz.symbol.comment.fold("")(_.raw))  
          case _ =>
        }
        super.traverseTree(tree)

    Traverser.traverseTree(root)(using reflect.rootContext)

  def result() = 
    val res = DDUnit(classes.result.toList, packages.result.toList)
    println(res)
    res


object Main:
  def main(args: Array[String]): Unit =
    val cp = args.headOption.getOrElse("/home/krzysiek/workspace/dotty-dokka/target/scala-0.25/classes")
    def listTastyFiles(f: File): Seq[File] = 
      val (files, dirs) = f.listFiles().partition(_.isFile)
      files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)
    
    val tastyFiles = cp.split(File.pathSeparatorChar).toList.flatMap(p => listTastyFiles(new File(p))).map(_.toString)

    val inspector = new DokkaTastyInspector()
    inspector.inspect(System.getProperty("java.class.path"), tastyFiles)

    
    val config = DottyDokkaConfig(inspector.result())
    new DokkaGenerator(config, DokkaConsoleLogger.INSTANCE).generate()
    println("Done")
