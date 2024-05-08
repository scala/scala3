package dotty.tools.sbtplugin

import sbt._
import xerial.sbt.pack.PackPlugin
import sbt.Keys._
import sbt.AutoPlugin
import sbt.PublishBinPlugin
import sbt.PublishBinPlugin.autoImport._
import sbt.io.Using
import sbt.util.CacheImplicits._

import scala.collection.mutable
import java.nio.file.Files
import versionhelpers.DottyVersion._

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters._

/** This local plugin provides ways of publishing a project classpath and library dependencies to
 * .a local repository */
object RepublishPlugin extends AutoPlugin {

  private object FileUtil {

    def tryMakeExecutable(path: Path): Boolean =
      try {
        val perms = Files.getPosixFilePermissions(path).asScala.toSet

        var newPerms = perms
        if (perms(PosixFilePermission.OWNER_READ))
          newPerms += PosixFilePermission.OWNER_EXECUTE
        if (perms(PosixFilePermission.GROUP_READ))
          newPerms += PosixFilePermission.GROUP_EXECUTE
        if (perms(PosixFilePermission.OTHERS_READ))
          newPerms += PosixFilePermission.OTHERS_EXECUTE

        if (newPerms != perms)
          Files.setPosixFilePermissions(
            path,
            newPerms.asJava
          )

        true
      }
      catch {
        case _: UnsupportedOperationException =>
          false
      }

  }

  override def trigger = allRequirements
  override def requires = super.requires && PublishBinPlugin && PackPlugin

  object autoImport {
    val republishProjectRefs = taskKey[Seq[ProjectRef]]("fetch the classpath deps from the project.")
    val republishLocalResolved = taskKey[Seq[ResolvedArtifacts]]("resolve local artifacts for distribution.")
    val republishAllResolved = taskKey[Seq[ResolvedArtifacts]]("Resolve the dependencies for the distribution")
    val republishClasspath = taskKey[Set[File]]("cache the dependencies for the distribution")
    val republishFetchLaunchers = taskKey[Set[File]]("cache the launcher deps for the distribution")
    val republishPrepareBin = taskKey[File]("prepare the bin directory, including launchers and scripts.")
    val republishBinDir = settingKey[File]("where to find static files for the bin dir.")
    val republishBinOverrides = settingKey[Seq[File]]("files to override those in bin-dir.")
    val republish = taskKey[File]("cache the dependencies and download launchers for the distribution")
    val republishRepo = settingKey[File]("the location to store the republished artifacts.")
    val republishLaunchers = settingKey[Seq[(String, String)]]("launchers to download. Sequence of (name, version, URL).")
  }

  import autoImport._

  case class SimpleModuleId(org: String, name: String, revision: String) {
    override def toString = s"$org:$name:$revision"
  }
  case class ResolvedArtifacts(id: SimpleModuleId, jar: File, pom: File)

  override val projectSettings: Seq[Def.Setting[_]] = Def.settings(
    republishLaunchers := Seq.empty,
    republishBinOverrides := Seq.empty,
    republishLocalResolved / republishProjectRefs := {
      val proj = thisProjectRef.value
      val deps = buildDependencies.value

      deps.classpathRefs(proj)
    },
    republishLocalResolved := Def.taskDyn {
      val deps = (republishLocalResolved / republishProjectRefs).value
      val publishAllLocalBin = deps.map({ d => ((d / publishLocalBin / packagedArtifacts)) }).join
      val resolveId = deps.map({ d => ((d / projectID)) }).join
      Def.task {
        val published = publishAllLocalBin.value
        val ids = resolveId.value

        ids.zip(published).map({ case (id, as) =>
          val simpleId = {
            val disabled = CrossVersion.disabled
            val name0 = id.crossVersion match {
              case cv: CrossVersion.Binary =>
                // projectID does not add binary suffix
                (s"${id.name}_${cv.prefix}${cv.suffix}3")
                  .ensuring(!id.name.endsWith("_3") && id.revision.startsWith("3."))
              case _ => id.name
            }
            SimpleModuleId(id.organization, name0, id.revision)
          }
          var jarOrNull: File = null
          var pomOrNull: File = null
          as.foreach({ case (a, f) =>
            if (a.`type` == "jar") {
              jarOrNull = f
            } else if (a.`type` == "pom") {
              pomOrNull = f
            }
          })
          assert(jarOrNull != null, s"Could not find jar for ${id}")
          assert(pomOrNull != null, s"Could not find pom for ${id}")
          ResolvedArtifacts(simpleId, jarOrNull, pomOrNull)
        })
      }
    }.value,
    republishAllResolved := {
      val localResolved = republishLocalResolved.value
      val report = (thisProjectRef / updateFull).value

      val found = mutable.Map.empty[SimpleModuleId, ResolvedArtifacts]
      val evicted = mutable.Set.empty[SimpleModuleId]

      localResolved.foreach({ resolved =>
        val simpleId = resolved.id
        if (simpleId.revision == dottyVersion) {
          evicted += simpleId.copy(revision = dottyNonBootstrappedVersion)
        }
        found(simpleId) = resolved
      })

      report.allModuleReports.foreach { mr =>
        val simpleId = {
          val id = mr.module
          SimpleModuleId(id.organization, id.name, id.revision)
        }

        if (!found.contains(simpleId) && !evicted(simpleId)) {
          var jarOrNull: File = null
          var pomOrNull: File = null
          mr.artifacts.foreach({ case (a, f) =>
            if (a.`type` == "jar" || a.`type` == "bundle") {
              jarOrNull = f
            } else if (a.`type` == "pom") {
              pomOrNull = f
            }
          })
          assert(jarOrNull != null, s"Could not find jar for ${simpleId}")
          if (pomOrNull == null) {
            val jarPath = jarOrNull.toPath
            // we found the jar, so assume we can resolve a sibling pom file
            val pomPath = jarPath.resolveSibling(jarPath.getFileName.toString.stripSuffix(".jar") + ".pom")
            assert(Files.exists(pomPath), s"Could not find pom for ${simpleId}")
            pomOrNull = pomPath.toFile
          }
          found(simpleId) = ResolvedArtifacts(simpleId, jarOrNull, pomOrNull)
        }

      }
      found.values.toSeq
    },
    republishClasspath := {
      val s = streams.value
      val resolved = republishAllResolved.value
      val cacheDir = republishRepo.value

      val log = s.log
      val mavenRepo = cacheDir / "maven2"
      IO.createDirectory(mavenRepo)
      resolved.map { ra =>
        log.info(s"[republish] publishing ${ra.id} to $mavenRepo...")
        val jar = ra.jar
        val pom = ra.pom

        val pathElems = ra.id.org.split('.').toVector :+ ra.id.name :+ ra.id.revision
        val artifactDir = pathElems.foldLeft(mavenRepo)(_ / _)
        IO.createDirectory(artifactDir)
        IO.copyFile(jar, artifactDir / jar.getName)
        IO.copyFile(pom, artifactDir / pom.getName)
        artifactDir
      }.toSet
    },
    republishFetchLaunchers := {
      val s = streams.value
      val log = s.log
      val repoDir = republishRepo.value
      val launcherVersions = republishLaunchers.value
      val libexec = republishPrepareBin.value

      val dlCache = repoDir / "cache"

      val store = s.cacheStoreFactory / "versions"

      def work(name: String, dest: File, launcher: String): File = {
        val (launcherURL, workFile, prefix, subPart) = {
          if (launcher.startsWith("gz+")) {
            IO.createDirectory(dlCache)
            val launcherURL = url(launcher.stripPrefix("gz+"))
            (launcherURL, dlCache / s"$name.gz", "gz", "")
          } else if (launcher.startsWith("zip+")) {
            IO.createDirectory(dlCache)
            val (urlPart, subPath) = launcher.split("!/") match {
              case Array(urlPart, subPath) => (urlPart, subPath)
              case _ =>
                throw new MessageOnlyException(s"[republish] Invalid zip+ URL, expected ! to mark subpath: $launcher")
            }
            val launcherURL = url(urlPart.stripPrefix("zip+"))
            (launcherURL, dlCache / s"$name.zip", "zip", subPath)
          } else {
            IO.createDirectory(libexec)
            (url(launcher), dest, "", "")
          }
        }
        IO.delete(workFile)
        Using.urlInputStream(launcherURL) { in =>
          log.info(s"[republish] Downloading $launcherURL to $workFile...")
          IO.transfer(in, workFile)
          log.info(s"[republish] Downloaded $launcherURL to $workFile...")
        }
        if (prefix == "gz") {
          IO.delete(dest)
          Using.fileInputStream(workFile) { in =>
            Using.gzipInputStream(in) { gzIn =>
              IO.transfer(gzIn, dest)
            }
          }
          log.info(s"[republish] uncompressed gz file $workFile to $dest...")
          FileUtil.tryMakeExecutable(dest.toPath) // TODO: we also need to copy to the bin directory so the archive makes it executable
          IO.delete(workFile)
        } else if (prefix == "zip") {
          IO.delete(dest)
          val files = IO.unzip(workFile, dlCache, new ExactFilter(subPart))
          val extracted = files.headOption.getOrElse(throw new MessageOnlyException(s"[republish] No files extracted from $workFile matching $subPart"))
          log.info(s"[republish] unzipped $workFile to $extracted...")
          IO.move(extracted, dest)
          log.info(s"[republish] moved $extracted to $dest...")
          FileUtil.tryMakeExecutable(dest.toPath) // TODO: we also need to copy to the bin directory so the archive makes it executable
          IO.delete(workFile)
        }
        dest
      }

      val allLaunchers = {
        for ((name, launcher) <- launcherVersions) yield {
          val dest = libexec / name

          val id = name.replaceAll("[^a-zA-Z0-9]", "_")

          val fetchAction = Tracked.inputChanged[String, File](store.make(id)) { (inChanged, launcher) =>
            if (inChanged || !Files.exists(dest.toPath)) {
              work(name, dest, launcher)
            } else {
              log.info(s"[republish] Using cached $launcher at $dest...")
              dest
            }
          }

          fetchAction(launcher)
        }
      }
      allLaunchers.toSet
    },
    republishPrepareBin := {
      val baseDir = baseDirectory.value
      val srcBin = republishBinDir.value
      val overrides = republishBinOverrides.value
      val repoDir = republishRepo.value

      val targetBin = repoDir / "bin"
      IO.copyDirectory(srcBin, targetBin)
      overrides.foreach { dir =>
        IO.copyDirectory(dir, targetBin, overwrite = true)
      }
      targetBin
    },
    republish := {
      val cacheDir = republishRepo.value
      val artifacts = republishClasspath.value
      val launchers = republishFetchLaunchers.value
      cacheDir
    }
  )
}
