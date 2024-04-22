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

/** This local plugin provides ways of publishing a project classpath and library dependencies to
 * .a local repository */
object RepublishPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = super.requires && PublishBinPlugin && PackPlugin

  object autoImport {
    val republishProjectRefs = taskKey[Seq[ProjectRef]]("fetch the classpath deps from the project.")
    val republishLocalResolved = taskKey[Seq[ResolvedArtifacts]]("resolve local artifacts for distribution.")
    val republishAllResolved = taskKey[Seq[ResolvedArtifacts]]("Resolve the dependencies for the distribution")
    val republishClasspath = taskKey[Set[File]]("cache the dependencies for the distribution")
    val republishFetchLaunchers = taskKey[Set[File]]("cache the launcher deps for the distribution")
    val republish = taskKey[File]("cache the dependencies and download launchers for the distribution")
    val republishRepo = settingKey[File]("the location to store the republished artifacts.")
    val republishLaunchers = settingKey[Seq[(String, String, URL)]]("launchers to download. Sequence of (name, version, URL).")
  }

  import autoImport._

  case class SimpleModuleId(org: String, name: String, revision: String) {
    override def toString = s"$org:$name:$revision"
  }
  case class ResolvedArtifacts(id: SimpleModuleId, jar: File, pom: File)

  override val projectSettings: Seq[Def.Setting[_]] = Def.settings(
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
        evicted += simpleId.copy(revision = simpleId.revision + "-nonbootstrapped")
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

      val etc = repoDir / "etc"

      val store = s.cacheStoreFactory / "versions"

      def work(dest: File, launcher: URL) = {
        IO.delete(dest)
        Using.urlInputStream(launcher) { in =>
          IO.createDirectory(etc)
          log.info(s"[republish] Downloading $launcher to $dest...")
          IO.transfer(in, dest)
          log.info(s"[republish] Downloaded $launcher to $dest...")
        }
        dest
      }

      val allLaunchers = {
        for ((name, version, launcher) <- launcherVersions) yield {
          val dest = etc / name

          val id = name.replaceAll("[^a-zA-Z0-9]", "_")

          val fetchAction = Tracked.inputChanged[String, File](store.make(id)) { (inChanged, version) =>
            if (inChanged || !Files.exists(dest.toPath)) {
              work(dest, launcher)
            } else {
              log.info(s"[republish] Using cached $launcher at $dest...")
              dest
            }
          }

          fetchAction(version)
        }
      }
      allLaunchers.toSet
    },
    republish := {
      val cacheDir = republishRepo.value
      val artifacts = republishClasspath.value
      val launchers = republishFetchLaunchers.value
      cacheDir
    }
  )
}
