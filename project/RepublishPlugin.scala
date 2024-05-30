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

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters._

/** This local plugin provides ways of publishing a project classpath and library dependencies to
 * .a local repository */
object RepublishPlugin extends AutoPlugin {

  /** copied from github.com/coursier/coursier */
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
    val republishFetchCoursier = taskKey[File]("cache the coursier.jar for resolving the local maven repo.")
    val republishPrepareBin = taskKey[File]("prepare the bin directory, including launchers and scripts.")
    val republishWriteExtraProps = taskKey[Option[File]]("write extra properties for the launchers.")
    val republishBinDir = settingKey[File]("where to find static files for the bin dir.")
    val republishCoursierDir = settingKey[File]("where to download the coursier launcher jar.")
    val republishBinOverrides = settingKey[Seq[File]]("files to override those in bin-dir.")
    val republish = taskKey[File]("cache the dependencies and download launchers for the distribution")
    val republishRepo = settingKey[File]("the location to store the republished artifacts.")
    val republishLaunchers = settingKey[Seq[(String, String)]]("launchers to download. Sequence of (name, URL).")
    val republishCoursier = settingKey[Seq[(String, String)]]("coursier launcher to download. Sequence of (name, URL).")
    val republishExtraProps = settingKey[Seq[(String, String)]]("extra properties for launchers.")
  }

  import autoImport._

  case class SimpleModuleId(org: String, name: String, revision: String) {
    override def toString = s"$org:$name:$revision"
  }
  case class ResolvedArtifacts(id: SimpleModuleId, jar: Option[File], pom: Option[File])

  private def republishResolvedArtifacts(resolved: Seq[ResolvedArtifacts], mavenRepo: File, logOpt: Option[Logger]): Set[File] = {
    IO.createDirectory(mavenRepo)
    resolved.map { ra =>
      for (log <- logOpt)
        log.info(s"[republish] publishing ${ra.id} to $mavenRepo...")
      val jarOpt = ra.jar
      val pomOpt = ra.pom

      assert(jarOpt.nonEmpty || pomOpt.nonEmpty, s"Neither jar nor pom found for ${ra.id}")

      val pathElems = ra.id.org.split('.').toVector :+ ra.id.name :+ ra.id.revision
      val artifactDir = pathElems.foldLeft(mavenRepo)(_ / _)
      IO.createDirectory(artifactDir)
      for (pom <- pomOpt) IO.copyFile(pom, artifactDir / pom.getName)
      for (jar <- jarOpt) IO.copyFile(jar, artifactDir / jar.getName)
      artifactDir
    }.toSet
  }

  private def coursierCmd(jar: File, cache: File, args: Seq[String]): Unit = {
    val jar0 = jar.getAbsolutePath.toString
    val javaHome = sys.props.get("java.home").getOrElse {
      throw new MessageOnlyException("java.home property not set")
    }
    val javaCmd = {
      val cmd = if (scala.util.Properties.isWin) "java.exe" else "java"
      (file(javaHome) / "bin" / cmd).getAbsolutePath
    }
    val env = Map("COURSIER_CACHE" -> cache.getAbsolutePath.toString)
    val cmdLine = Seq(javaCmd, "-jar", jar0) ++ args
    // invoke cmdLine with env
    val p = new ProcessBuilder(cmdLine: _*).inheritIO()
    p.environment().putAll(env.asJava)
    val proc = p.start()
    proc.waitFor()
    if (proc.exitValue() != 0)
      throw new MessageOnlyException(s"Error running coursier.jar with args ${args.mkString(" ")}")
  }

  private def coursierFetch(coursierJar: File, log: Logger, cacheDir: File, localRepo: File, libs: Seq[String]): Unit = {
    val localRepoArg = {
      val path = localRepo.getAbsolutePath
      if (scala.util.Properties.isWin) {
        val path0 = path.replace('\\', '/')
        s"file:///$path0" // extra root slash for Windows paths
      }
      else
        s"file://$path"
    }

    IO.createDirectory(cacheDir)
    for (lib <- libs) {
      log.info(s"[republish] Fetching $lib with coursier.jar...")
      coursierCmd(coursierJar, cacheDir,
        Seq(
          "fetch",
          "--repository", localRepoArg,
          lib
        )
      )
    }
  }

  /**Resolve the transitive library dependencies of `libs` to `csrCacheDir`.
   */
  private def resolveLibraryDeps(
      coursierJar: File,
      log: Logger,
      csrCacheDir: File,
      localRepo: File,
      resolvedLocal: Seq[ResolvedArtifacts]): Seq[ResolvedArtifacts] = {

    // publish the local artifacts to the local repo, so coursier can resolve them
    republishResolvedArtifacts(resolvedLocal, localRepo, logOpt = None)

    coursierFetch(coursierJar, log, csrCacheDir, localRepo, resolvedLocal.map(_.id.toString))

    val maven2Root = java.nio.file.Files.walk(csrCacheDir.toPath)
      .filter(_.getFileName.toString == "maven2")
      .findFirst()
      .orElseThrow(() => new MessageOnlyException(s"Could not find maven2 directory in $csrCacheDir"))

    def pathToArtifact(p: Path): ResolvedArtifacts = {
      // relative path from maven2Root
      val lastAsString = p.getFileName.toString
      val relP = maven2Root.relativize(p)
      val parts = relP.iterator().asScala.map(_.toString).toVector
      val (orgParts :+ name :+ rev :+ _) = parts
      val id = SimpleModuleId(orgParts.mkString("."), name, rev)
      if (lastAsString.endsWith(".jar")) {
        ResolvedArtifacts(id, Some(p.toFile), None)
      } else {
        ResolvedArtifacts(id, None, Some(p.toFile))
      }
    }

    java.nio.file.Files.walk(maven2Root)
      .filter(p => {
        val lastAsString = p.getFileName.toString
        lastAsString.endsWith(".pom") || lastAsString.endsWith(".jar")
      })
      .map[ResolvedArtifacts](pathToArtifact(_))
      .iterator()
      .asScala
      .toSeq
  }

  private def fetchFilesTask(
      libexecT: Def.Initialize[Task[File]],
      srcs: SettingKey[Seq[(String, String)]],
      strict: Boolean) = Def.task[Set[File]] {
    val s = streams.value
    val log = s.log
    val repoDir = republishRepo.value
    val launcherVersions = srcs.value
    val libexec = libexecT.value

    val dlCache = s.cacheDirectory / "republish-launchers"

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
        IO.delete(workFile)
      } else if (prefix == "zip") {
        IO.delete(dest)
        val files = IO.unzip(workFile, dlCache, new ExactFilter(subPart))
        val extracted = files.headOption.getOrElse(throw new MessageOnlyException(s"[republish] No files extracted from $workFile matching $subPart"))
        log.info(s"[republish] unzipped $workFile to $extracted...")
        IO.move(extracted, dest)
        log.info(s"[republish] moved $extracted to $dest...")
        IO.delete(workFile)
      }
      FileUtil.tryMakeExecutable(dest.toPath)
      dest
    }

    val allLaunchers = {
      if (strict && launcherVersions.isEmpty)
        throw new MessageOnlyException(s"[republish] No launchers to fetch, check the build configuration for ${srcs.key.label}.")

      for ((name, launcher) <- launcherVersions) yield {
        val dest = libexec / name

        val id = name.replaceAll("[^a-zA-Z0-9]", "_")

        val fetchAction = Tracked.inputChanged[String, File](store.make(id)) { (inChanged, launcher) =>
          if (inChanged || !Files.exists(dest.toPath)) {
            work(name, dest, launcher)
          } else {
            log.info(s"[republish] Using cached $name launcher ($launcher).")
            dest
          }
        }

        fetchAction(launcher)
      }
    }
    allLaunchers.toSet
  }

  override val projectSettings: Seq[Def.Setting[_]] = Def.settings(
    republishCoursierDir := republishRepo.value / "coursier",
    republishLaunchers := Seq.empty,
    republishCoursier := Seq.empty,
    republishBinOverrides := Seq.empty,
    republishExtraProps := Seq.empty,
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
          ResolvedArtifacts(simpleId, Some(jarOrNull), Some(pomOrNull))
        })
      }
    }.value,
    republishAllResolved := {
      val resolvedLocal = republishLocalResolved.value
      val coursierJar = republishFetchCoursier.value
      val report = (thisProjectRef / updateFull).value
      val s = streams.value
      val lm = (republishAllResolved / dependencyResolution).value
      val cacheDir = republishRepo.value

      val log = s.log
      val csrCacheDir = s.cacheDirectory / "csr-cache"
      val localRepo = s.cacheDirectory / "localRepo" / "maven2"

      // resolve the transitive dependencies of the local artifacts
      val resolvedLibs = resolveLibraryDeps(coursierJar, log, csrCacheDir, localRepo, resolvedLocal)

      // the combination of local artifacts and resolved transitive dependencies
      val merged =
        (resolvedLocal ++ resolvedLibs).groupBy(_.id).values.map(_.reduce { (ra1, ra2) =>
          val jar = ra1.jar.orElse(ra2.jar)
          val pom = ra1.pom.orElse(ra2.pom)
          ResolvedArtifacts(ra1.id, jar, pom)
        })

      merged.toSeq
    },
    republishClasspath := {
      val s = streams.value
      val resolved = republishAllResolved.value
      val cacheDir = republishRepo.value
      republishResolvedArtifacts(resolved, cacheDir / "maven2", logOpt = Some(s.log))
    },
    republishFetchLaunchers := {
      fetchFilesTask(republishPrepareBin, republishLaunchers, strict = true).value
    },
    republishFetchCoursier := {
      fetchFilesTask(republishCoursierDir.toTask, republishCoursier, strict = true).value.head
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
    republishWriteExtraProps := {
      val s = streams.value
      val log = s.log
      val extraProps = republishExtraProps.value
      if (extraProps.isEmpty) {
        log.info("[republish] No extra properties to write.")
        None
      }
      else {
        val repoDir = republishRepo.value
        val propsFile = repoDir / "etc" / "EXTRA_PROPERTIES"
        log.info(s"[republish] Writing extra properties to $propsFile...")
        Using.fileWriter()(propsFile) { writer =>
          extraProps.foreach { case (k, v) =>
            writer.write(s"$k:=$v\n")
          }
        }
        Some(propsFile)
      }
    },
    republish := {
      val cacheDir = republishRepo.value
      val artifacts = republishClasspath.value
      val launchers = republishFetchLaunchers.value
      val extraProps = republishWriteExtraProps.value
      cacheDir
    }
  )
}
