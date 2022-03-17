import sbt._
import Build._

object ScaladocGeneration {
  def generateCommand(config: GenerationConfig): String =
    " " + config.serialize

  sealed trait Arg[T] {
    def key: String
    def value: T

    def serialize: String =
      value match {
        case s: String => s"$key ${escape(s)}"
        case true => s"$key"
        case list: List[_] => s"$key:${list.map(x => escape(x.toString)).mkString(",")}"
        case _ =>
          println(s"Unsupported setting: $key -> $value")
          ""
      }

    private def escape(s: String) = if (s.contains(" ")) s""""$s"""" else s
  }
  case class ProjectName(value: String) extends Arg[String] {
    def key: String = "-project"
  }

  case class OutputDir(value: String) extends Arg[String] {
    def key: String = "-d"
  }

  case class ProjectVersion(value: String) extends Arg[String] {
    def key: String = "-project-version"
  }

  case class ProjectLogo(value: String) extends Arg[String] {
    def key: String = "-project-logo"
  }

  case class ProjectFooter(value: String) extends Arg[String] {
    def key: String = "-project-footer"
  }

  case class SourceLinks(value: List[String]) extends Arg[List[String]] {
    def key: String = "-source-links"
  }

  case class CommentSyntax(value: List[String]) extends Arg[List[String]] {
    def key: String = "-comment-syntax"
  }

  case class Revision(value: String) extends Arg[String] {
    def key: String = "-revision"
  }

  case class ExternalMappings(value: List[String]) extends Arg[List[String]] {
    def key: String = "-external-mappings"
  }

  case class SocialLinks(value: List[String]) extends Arg[List[String]] {
    def key: String = "-social-links"
  }

  case class SkipPackages(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-packages"
  }

  case class SkipById(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-by-id"
  }

  case class SkipByRegex(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-by-regex"
  }

  case class DocRootContent(value: String) extends Arg[String] {
    def key: String = "-doc-root-content"
  }

  case class Author(value: Boolean) extends Arg[Boolean] {
    def key: String = "-author"
  }

  case class Groups(value: Boolean) extends Arg[Boolean] {
    def key: String = "-groups"
  }

  case class DocCanonicalBaseUrl(value: String) extends Arg[String] {
    def key: String = "-doc-canonical-base-url"
  }

  case class SiteRoot(value: String) extends Arg[String] {
    def key: String = "-siteroot"
  }

  case class NoLinkWarnings(value: Boolean) extends Arg[Boolean] {
    def key: String = "-no-link-warnings"
  }

  case class VersionsDictionaryUrl(value: String) extends Arg[String] {
    def key: String = "-versions-dictionary-url"
  }

  case class DocumentSyntheticTypes(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Ydocument-synthetic-types"
  }

  case class SnippetCompiler(value: List[String]) extends Arg[List[String]] {
    def key: String = "-snippet-compiler"
  }

  case class GenerateInkuire(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Ygenerate-inkuire"
  }

  case class ApiSubdirectory(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Yapi-subdirectory"
  }

  case class ScastieConfiguration(value: String) extends Arg[String] {
    def key: String = "-scastie-configuration"
  }

  case class DefaultTemplate(value: String) extends Arg[String] {
    def key: String = "-default-template"
  }

  case class UseJavacp(value: Boolean) extends Arg[Boolean] {
    def key: String = "-usejavacp"
  }

  import _root_.scala.reflect._

  trait GenerationConfig {
    def get[T <: Arg[_] : ClassTag]: Option[T]
    def add[T <: Arg[_]](arg: T): GenerationConfig
    def remove[T <: Arg[_]: ClassTag]: GenerationConfig
    def withTargets(targets: Seq[String]): GenerationConfig
    def serialize: String
  }

  object GenerationConfig {
    def apply(): GenerationConfig = GenerationConfigImpl(Seq.empty, Seq.empty)
    def apply(targets: List[String], args: Arg[_]*): GenerationConfig = args.foldLeft(GenerationConfig()) { (config, elem) =>
      config.add(elem)
    }
    private case class GenerationConfigImpl(targets: Seq[String], args: Seq[Arg[_]]) extends GenerationConfig {
      override def add[T <: Arg[_]](arg: T): GenerationConfig = {
        implicit val tag: ClassTag[T] = ClassTag(arg.getClass)
        val (removedElem, argsWithoutElem) = argsWithout[T](tag)
        removedElem.foreach(elem => println(s"$elem has been overwritten by $arg"))
        GenerationConfigImpl(targets, argsWithoutElem :+ arg)
      }
      override def remove[T <: Arg[_] : ClassTag]: GenerationConfig = {
        GenerationConfigImpl(targets, argsWithout[T]._2)
      }

      override def get[T <: Arg[_] : ClassTag]: Option[T] = {
        val tag = implicitly[ClassTag[T]]
        args.collect {
          case tag(t) => t
        }.headOption
      }

      override def withTargets(targets: Seq[String]) = copy(targets = targets)
      override def serialize: String = (
        args
        .map(_.serialize)
         ++ targets
      ).mkString(" ")

      private def argsWithout[T <: Arg[_]](
        implicit tag: ClassTag[T]
      ): (Option[T], Seq[Arg[_]]) = args.foldLeft[(Option[T], Seq[Arg[_]])]((None, Seq.empty)) {
        case ((removedElem, rest), tag(t)) => (Some(t), rest)
        case ((removedElem, rest), elem) => (removedElem, rest :+ elem)
      }
    }
  }

}
