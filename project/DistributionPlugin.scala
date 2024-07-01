import com.typesafe.sbt.packager.Keys.stage
import com.typesafe.sbt.packager.universal.UniversalPlugin
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.Universal
import sbt.*

/**
 * @author Hamza REMMAL (https://github.com/hamzaremmal/)
 */
object DistributionPlugin extends AutoPlugin {

  override def trigger = allRequirements

  override def requires =
    super.requires && UniversalPlugin // Require the Universal Plugin to

  object autoImport {
    val `universal_project`     = settingKey[Project]("???")
    val `linux-aarch64_project` = settingKey[Project]("???")
    val `linux-x86_64_project`  = settingKey[Project]("???")
    val `mac-aarch64_project`   = settingKey[Project]("???")
    val `win-x86_64_project`    = settingKey[Project]("???")


    // ========================== TASKS TO GENERATE THE FOLDER PACKAGE ============================
    val `pack-universal`     =
      taskKey[File]("Generate the package with the universal binaries (folder)")
    val `pack_linux-aarch64` =
      taskKey[File]("Generate the package with the linux-aarch64 binaries (folder)")
    val `pack_linux-x86_64`  =
      taskKey[File]("Generate the package with the linux-x86_64 binaries (folder)")
    val `pack_mac-aarch64`   =
      taskKey[File]("Generate the package with the mac-aarch64 binaries (folder)")
    val `pack_mac-x86_64`    =
      taskKey[File]("Generate the package with the mac-x86_64 binaries (folder)")
    val `pack_win-x86_64`    =
      taskKey[File]("Generate the package with the linux-x86_64 binaries (folder)")
  }

  import autoImport.*

  override def projectSettings = Def.settings(
    `pack-universal` := (`universal_project` / Universal./(stage)).value ,
    `pack_linux-aarch64` := ???,
    `pack_linux-x86_64` := ???,
    `pack_mac-aarch64` := ???,
    `pack_mac-x86_64` := ???,
    `pack_win-x86_64` := ???
  )

}
