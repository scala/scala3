package sbt

import java.nio.file.{ FileAlreadyExistsException, Files }

import sbt.given
import sbt.Keys.*
import sbt.internal.librarymanagement.IvyXml
import sbt.{toFile, toFileRef}
import xsbti.{FileConverter, HashedVirtualFileRef}

/** This local plugin provides ways of publishing just the binary jar. */
object PublishBinPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val publishLocalBin = taskKey[Unit]("")
    val publishLocalBinConfig = taskKey[PublishConfiguration]("")
  }
  import autoImport._

  private val dummyDoc = taskKey[HashedVirtualFileRef]("").withRank(Int.MaxValue)
  override val globalSettings = Seq(publishLocalBin := (()))

  override val projectSettings: Seq[Def.Setting[?]] = Def.settings(
    publishLocalBin := Classpaths.publishOrSkip(publishLocalBinConfig, publishLocal / skip).value,
    publishLocalBinConfig := Def.uncached {
      given FileConverter = fileConverter.value
      Classpaths.publishConfig(
        false, // publishMavenStyle.value,
        Classpaths.deliverPattern(crossTarget.value),
        if (isSnapshot.value) "integration" else "release",
        ivyConfigurations.value.map(c => ConfigRef(c.name)).toVector,
        (publishLocalBin / packagedArtifacts).value.toVector.map { case (a, f) => a -> f.toFile },
        (publishLocalBin / checksums).value.toVector,
        logging = ivyLoggingLevel.value,
        overwrite = isSnapshot.value
      )
    },
    publishLocalBinConfig := Def.uncached(
      publishLocalBinConfig
      .dependsOn(
        Def.taskDyn {
          Def.task {
            val currentProject = {
              val proj = csrProject.value
              val publications = csrPublications.value
              proj.withPublications(publications)
            }
            IvyXml.writeFiles(
              currentProject,
              None,
              ivySbt.value,
              streams.value.log,
              update.value.allModules,
            )
          }
        }
      )
      .value
    ),
    dummyDoc := Def.uncached {
      given FileConverter = fileConverter.value
      val dummyFile = streams.value.cacheDirectory / "doc.jar"
      try {
        Files.createDirectories(dummyFile.toPath.getParent)
        Files.createFile(dummyFile.toPath)
      } catch { case _: FileAlreadyExistsException => }
      dummyFile.toFileRef
    },
    dummyDoc / packagedArtifact := Def.uncached(
      (Compile / packageDoc / artifact).value -> dummyDoc.value
    ),
    publishLocalBin / packagedArtifacts := Def.uncached(
      Classpaths
        .packaged(Seq(Compile / packageBin, Compile / packageSrc, makePom, dummyDoc))
        .value
    ),
  )
}
