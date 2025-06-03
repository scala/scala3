package sbt

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{ FileAlreadyExistsException, Files }

import org.apache.ivy.core.module.id.ModuleRevisionId
import sbt.Keys._
import sbt.internal.librarymanagement.{ IvySbt, IvyXml }

/** This local plugin provides ways of publishing just the binary jar. */
object PublishBinPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val publishLocalBin = taskKey[Unit]("")
    val publishLocalBinConfig = taskKey[PublishConfiguration]("")
  }
  import autoImport._

  private val dummyDoc = taskKey[File]("").withRank(Int.MaxValue)
  override val globalSettings = Seq(publishLocalBin := (()))

  override val projectSettings: Seq[Def.Setting[_]] = Def settings (
    publishLocalBin := Classpaths.publishOrSkip(publishLocalBinConfig, publishLocal / skip).value,
    publishLocalBinConfig := Classpaths.publishConfig(
      false, // publishMavenStyle.value,
      Classpaths.deliverPattern(crossTarget.value),
      if (isSnapshot.value) "integration" else "release",
      ivyConfigurations.value.map(c => ConfigRef(c.name)).toVector,
      (publishLocalBin / packagedArtifacts).value.toVector,
      (publishLocalBin / checksums).value.toVector,
      logging = ivyLoggingLevel.value,
      overwrite = isSnapshot.value
    ),
    publishLocalBinConfig := publishLocalBinConfig
      .dependsOn(
        // Copied from sbt.internal.
        Def.taskDyn {
          val doGen = useCoursier.value
          if (doGen)
            Def.task {
              val currentProject = {
                val proj = csrProject.value
                val publications = csrPublications.value
                proj.withPublications(publications)
              }
              IvyXml.writeFiles(currentProject, None, ivySbt.value, streams.value.log)
            } else
            Def.task(())
        }
      )
      .value,
    dummyDoc := {
      val dummyFile = streams.value.cacheDirectory / "doc.jar"
      try {
        Files.createDirectories(dummyFile.toPath.getParent)
        Files.createFile(dummyFile.toPath)
      } catch { case _: FileAlreadyExistsException => }
      dummyFile
    },
    dummyDoc / packagedArtifact := (Compile / packageDoc / artifact).value -> dummyDoc.value,
    publishLocalBin / packagedArtifacts :=
      Classpaths
        .packaged(Seq(Compile / packageBin, Compile / packageSrc, makePom, dummyDoc))
        .value
  )
}
