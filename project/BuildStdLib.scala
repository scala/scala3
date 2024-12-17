import sbt._
import sbt.Keys._
import java.util.jar.JarFile
import java.nio.file.{Files, Paths, StandardCopyOption}

object BuildStdLib extends AutoPlugin {

  object autoImport {
    val library = settingKey[ModuleID]("The jar dependency to copy.")
    val jarExtractTarget = settingKey[File]("The target directory to extract jar content.")
    val copyJarContent = taskKey[Unit]("Copy the content of the specified jar to the target directory.")
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    copyJarContent := {
      val log = streams.value.log
      val jarDep = library.value
      val targetDir = jarExtractTarget.value

      val jarFiles = update.value.matching(moduleFilter(organization = jarDep.organization, name = jarDep.name))

      if (jarFiles.isEmpty) {
        log.error(s"Jar not found for dependency: ${jarDep}")
      } else {
        val jarFile = jarFiles.head
        log.info(s"Extracting ${jarFile} to ${targetDir}")

        extractJarContent(jarFile, targetDir, log)
      }
    }
  )

  private def extractJarContent(jarFile: File, targetDir: File, log: Logger): Unit = {
    val jar = new JarFile(jarFile)
    try {
      jar.entries().asIterator().forEachRemaining { entry =>
        val entryPath = targetDir.toPath.resolve(entry.getName)
        if (entry.isDirectory) {
          Files.createDirectories(entryPath)
        } else {
          Files.createDirectories(entryPath.getParent)
          Files.copy(jar.getInputStream(entry), entryPath, StandardCopyOption.REPLACE_EXISTING)
        }
      }
      log.info(s"Extraction complete: ${targetDir}")
    } finally {
      jar.close()
    }
  }
}
