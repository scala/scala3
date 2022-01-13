package dotty.communitybuild.sbtplugin

import sbt._
import sbt.Keys._
import sbt.librarymanagement.LibraryManagementCodec._
import sjsonnew.support.scalajson.unsafe.{ Converter, CompactPrinter, Parser }
import scala.util.{ Try, Failure }

/** This plugin provides automatic dependency overrides for projects in the
 *  community build. In doing so, we permit the projects in the build to
 *  depend on arbitrary versions of other projects in the build, and the
 *  version alignment is handled here.
 */
object CommunityBuildPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  object autoImport {
    val isForwardCompatProject = settingKey[Boolean]("Is it a project used for testing forward binary compatibility?")
  }

  import autoImport._

  override val globalSettings: Seq[Setting[_]] = Seq(
    isForwardCompatProject := false
  )

  override val projectSettings: Seq[Setting[_]] = Seq(
    publishLocal := Def.taskDyn {
      val pubLocalResult = publishLocal.value
      Def.task {
        if (artifacts.value.nonEmpty && !(publish / skip).value)
          CommunityBuildDependencies.publish(projectID.value)
        pubLocalResult
      }
    }.value,
    projectID := {
      val id = projectID.value
      if (isForwardCompatProject.value) {
        val revision = if (id.revision.endsWith("-SNAPSHOT"))
          id.revision.replace("-SNAPSHOT", "-forward-compat-SNAPSHOT")
        else
          id.revision + "-forward-compat"
        id.withRevision(revision)
      } else
        id
    }
  )

  override val buildSettings: Seq[Setting[_]] = Seq(
    dependencyOverrides ++= {
      if (scalaVersion.value.startsWith("3.")) {
        val forwardCompatFilter: ModuleID => Boolean = if (isForwardCompatProject.value) (_.revision.contains("-forward-compat")) else (!_.revision.contains("-forward-compat"))
        val stdlibOverrides = Seq(
          scalaOrganization.value %% "scala3-library" % scalaVersion.value,
          scalaOrganization.value %% "scala3-library_sjs1" % scalaVersion.value,
        )
        CommunityBuildDependencies.allOverrides(sLog.value).filter(forwardCompatFilter) ++ stdlibOverrides
      } else Nil
    }
  )
}

object CommunityBuildDependencies {
  private val communityBuildDir = Path(sys.props("dotty.communitybuild.dir"))
  private val depsFile = communityBuildDir / "dotty-community-build-deps"

  /** Publish dependency override data for a module in the community build.
   *  This appends a single entry to the tracking file.
   */
  def publish(moduleID: ModuleID): Unit = {
    val line = encode(sanitized(moduleID)) + "\n"
    synchronized { IO.append(depsFile, line) }
  }

  /** Returns all currently tracked dependency overrides. */
  def allOverrides(log: Logger): List[ModuleID] = load(log)

  /** Load all entries from the dependency tracking file. */
  private def load(log: Logger): List[ModuleID] = {
    def logError(line: String): PartialFunction[Throwable, Try[ModuleID]] = {
      case ex: Throwable =>
        log.error(ex.toString())
        log.error(s"while parsing input: $line")
        Failure(ex)
    }

    log.info(s"Loading dependency tracking file $depsFile")
    try {
      val lines = synchronized { IO.readLines(depsFile) }
      lines.map { s => decode(s).recoverWith(logError(s)).toOption }.flatten
    } catch {
      case _: java.io.FileNotFoundException =>
        log.info(s"Dependency tracking file $depsFile does not exist")
        Nil
    }
  }

  private def encode(m: ModuleID): String =
    CompactPrinter(Converter.toJsonUnsafe(m))

  // It seems that badly formatted JSON will throw, e.g.
  // sjsonnew.shaded.org.typelevel.jawn.IncompleteParseException: exhausted input
  // But missing/misnamed fields will not and the malformed entry will be loaded
  private def decode(s: String): Try[ModuleID] =
    Parser.parseFromString(s)
      .flatMap(Converter.fromJson[ModuleID])

  // preserve only organization, name, revision, and crossVersion
  private def sanitized(m: ModuleID): ModuleID =
    ModuleID(m.organization, m.name, m.revision).withCrossVersion(m.crossVersion)
}
