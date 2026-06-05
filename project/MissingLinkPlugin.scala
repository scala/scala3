package ch.epfl.scala.sbtmissinglink

import sbt._
import sbt.Keys._
import sbt.librarymanagement.ModuleFilter
import sbt.plugins.JvmPlugin

import sbtcompat.PluginCompat

import java.io.FileInputStream

import scala.jdk.CollectionConverters.*
import scala.collection.immutable.Map

import com.spotify.missinglink.{ArtifactLoader, Conflict, ConflictChecker, Java9ModuleLoader}
import com.spotify.missinglink.Conflict.ConflictCategory
import com.spotify.missinglink.datamodel.{
  Artifact,
  ArtifactBuilder,
  ArtifactName,
  ClassTypeDescriptor,
  DeclaredClass,
  Dependency
}
import xsbti.FileConverter

object MissingLinkPlugin extends AutoPlugin {

  object autoImport {

    final case class IgnoredPackage(name: String, ignoreSubpackages: Boolean = true)
        extends PackageFilter {
      override def apply(packageName: String): Boolean =
        packageName != name && (!ignoreSubpackages || !packageName.startsWith(s"$name."))
    }
    private[sbtmissinglink] implicit object IgnoredPackages extends PackageFilters[IgnoredPackage] {
      def apply(name: String)(filters: Seq[IgnoredPackage]): Boolean = filters.forall(_.apply(name))
    }

    final case class TargetedPackage(name: String, targetSubpackages: Boolean = true)
        extends PackageFilter {
      override def apply(packageName: String): Boolean =
        packageName == name || (targetSubpackages && packageName.startsWith(s"$name."))
    }

    private[sbtmissinglink] implicit object TargetedPackages
        extends PackageFilters[TargetedPackage] {
      def apply(name: String)(filters: Seq[TargetedPackage]): Boolean =
        filters.exists(_.apply(name))
    }

    @transient
    val missinglinkCheck: TaskKey[Unit] =
      taskKey[Unit]("Run the missinglink checks")

    val missinglinkFailOnConflicts: SettingKey[Boolean] =
      settingKey[Boolean]("Fail the build if any conflicts are found")

    val missinglinkScanDependencies: SettingKey[Boolean] =
      settingKey[Boolean]("Also scan all dependencies")

    val missinglinkIgnoreSourcePackages: SettingKey[Seq[IgnoredPackage]] =
      settingKey[Seq[IgnoredPackage]](
        "Optional list of packages to ignore conflicts where the source of the conflict " +
          "is in one of the specified packages."
      )

    val missinglinkTargetSourcePackages: SettingKey[Seq[TargetedPackage]] =
      settingKey[Seq[TargetedPackage]](
        "Optional list of source packages to specifically target conflicts in. " +
          "Cannot be used with missinglinkIgnoreSourcePackages."
      )

    val missinglinkIgnoreDestinationPackages: SettingKey[Seq[IgnoredPackage]] =
      settingKey[Seq[IgnoredPackage]](
        "Optional list of packages to ignore conflicts where the destination/called-side " +
          "of the conflict is in one of the specified packages."
      )

    val missinglinkTargetDestinationPackages: SettingKey[Seq[TargetedPackage]] =
      settingKey[Seq[TargetedPackage]](
        "Optional list of source packages to specifically target conflicts in. " +
          "Cannot be used with missinglinkIgnoreDestinationPackages."
      )

    val missinglinkExcludedDependencies =
      settingKey[Seq[ModuleFilter]]("Dependencies that are excluded from analysis")
  }

  import autoImport._

  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  // Make it easy to throttle the concurrency of running missing-link on multiple projects, it consumes a lot of memory
  val missinglinkConflictsTag = Tags.Tag("missinglinkConflicts")

  val configSettings: Seq[Setting[?]] = Def.settings(
    missinglinkCheck := Def
      .task {
        val log = streams.value.log

        implicit val converter: FileConverter = fileConverter.value
        val cp = fullClasspath.value
        val classDir = (Compile / classDirectory).value
        val failOnConflicts = missinglinkFailOnConflicts.value
        val scanDependencies = missinglinkScanDependencies.value
        assert(
          missinglinkIgnoreSourcePackages.value.isEmpty || missinglinkTargetSourcePackages.value.isEmpty,
          "ignoreSourcePackages and targetSourcePackages cannot be defined in the same project."
        )

        assert(
          missinglinkIgnoreDestinationPackages.value.isEmpty || missinglinkTargetDestinationPackages.value.isEmpty,
          "ignoreDestinationPackages and targetDestinationPackages cannot be defined in the same project."
        )

        val filter =
          missinglinkExcludedDependencies.value.foldLeft[ModuleFilter](_ => true)((k, v) => k - v)

        val conflicts = loadArtifactsAndCheckConflicts(cp, classDir, scanDependencies, filter, log)

        val conflictFilters = filterConflicts(
          missinglinkIgnoreSourcePackages.value,
          missinglinkIgnoreSourcePackages,
          log,
          "source",
          _.fromClass,
        ) andThen filterConflicts(
          missinglinkTargetSourcePackages.value,
          missinglinkTargetSourcePackages,
          log,
          "source",
          _.fromClass,
        ) andThen filterConflicts(
          missinglinkIgnoreDestinationPackages.value,
          missinglinkIgnoreDestinationPackages,
          log,
          "destination",
          _.targetClass,
        ) andThen filterConflicts(
          missinglinkTargetDestinationPackages.value,
          missinglinkTargetDestinationPackages,
          log,
          "destination",
          _.targetClass,
        )

        val filteredConflicts = conflictFilters(conflicts)

        if (filteredConflicts.nonEmpty) {
          val initialTotal = conflicts.length
          val filteredTotal = filteredConflicts.length

          val diffMessage = if (initialTotal != filteredTotal) {
            s"($initialTotal conflicts were found before applying filters)"
          } else {
            ""
          }

          log.info(s"$filteredTotal conflicts found! $diffMessage")

          outputConflicts(filteredConflicts, log)

          if (failOnConflicts)
            throw new MessageOnlyException(s"There were $filteredTotal conflicts")
        } else {
          log.info("No conflicts found")
        }
      }
      .tag(missinglinkConflictsTag)
      .value,
  )

  override def globalSettings: Seq[Def.Setting[?]] = Seq(
    missinglinkFailOnConflicts := true,
    missinglinkScanDependencies := false,
    missinglinkIgnoreSourcePackages := Nil,
    missinglinkTargetSourcePackages := Nil,
    missinglinkIgnoreDestinationPackages := Nil,
    missinglinkTargetDestinationPackages := Nil,
    missinglinkExcludedDependencies := Nil,
  )

  override def projectSettings: Seq[Setting[?]] = {
    inConfig(Compile)(configSettings) ++
      inConfig(Runtime)(configSettings) ++
      inConfig(Test)(configSettings)
  }

  private def loadArtifactsAndCheckConflicts(
      cp: Classpath,
      classDirectory: File,
      scanDependencies: Boolean,
      excluded: ModuleFilter,
      log: Logger
  )(implicit
      converter: FileConverter
  ): Seq[Conflict] = {

    val runtimeProjectArtifacts = constructArtifacts(cp, log)

    // also need to load JDK classes from the bootstrap classpath
    val bootstrapArtifacts = loadBootstrapArtifacts(bootClasspathToUse(log), log)

    val allArtifacts = runtimeProjectArtifacts.map(_.artifact) ++ bootstrapArtifacts

    val runtimeArtifactsAfterExclusions = runtimeProjectArtifacts
      .filter(f => f.module.fold(true)(excluded))
      .map(_.artifact)

    val projectArtifact =
      if (scanDependencies)
        classesToArtifact(runtimeArtifactsAfterExclusions.flatMap(_.classes.asScala).toMap)
      else
        toArtifact(classDirectory)

    if (projectArtifact.classes().isEmpty()) {
      log.warn(
        "No classes found in project build directory" +
          " - did you run 'sbt compile' first?"
      )
    }

    log.debug("Checking for conflicts starting from " + projectArtifact.name().name())
    log.debug("Artifacts included in the project: ")
    for (artifact <- runtimeArtifactsAfterExclusions) {
      log.debug("    " + artifact.name().name())
    }

    val conflictChecker = new ConflictChecker

    val conflicts =
      conflictChecker.check(
        projectArtifact,
        runtimeArtifactsAfterExclusions.asJava,
        allArtifacts.asJava
      )

    conflicts.asScala.toSeq
  }

  private def toArtifact(outputDirectory: File): Artifact = {
    val classes =
      (outputDirectory ** "*.class")
        .get()
        .map(loadClass)
        .map(c => c.className() -> c)
        .toMap

    classesToArtifact(classes)
  }

  private def classesToArtifact(classes: Map[ClassTypeDescriptor, DeclaredClass]): Artifact = {
    new ArtifactBuilder()
      .name(new ArtifactName("project"))
      .classes(classes.asJava)
      .build()
  }

  private def loadClass(f: File): DeclaredClass = {
    val is = new FileInputStream(f)
    try com.spotify.missinglink.ClassLoader.load(is)
    finally is.close()
  }

  private def loadBootstrapArtifacts(bootstrapClasspath: String, log: Logger)(implicit
      converter: FileConverter
  ): List[Artifact] = {
    if (bootstrapClasspath == null) {
      Java9ModuleLoader.getJava9ModuleArtifacts((s, ex) => log.warn(s)).asScala.toList
    } else {
      val cp = bootstrapClasspath
        .split(System.getProperty("path.separator"))
        .map(f => Attributed.blank(PluginCompat.toFileRef(file(f))))
        .toList

      constructArtifacts(cp, log).map(_.artifact)
    }
  }

  private def bootClasspathToUse(log: Logger): String = {
    /*if (this.bootClasspath != null) {
      log.debug("using configured boot classpath: " + this.bootClasspath);
      this.bootClasspath;
    } else {*/
    val bootClasspath = System.getProperty("sun.boot.class.path")
    log.debug("derived bootclasspath: " + bootClasspath)
    bootClasspath
    /*}*/
  }

  private def constructArtifacts(cp: Classpath, log: Logger)(implicit
      converter: FileConverter
  ): List[ModuleArtifact] = {
    val artifactLoader = new ArtifactLoader

    def isValid(entry: File): Boolean =
      (entry.isFile() && entry.getPath().endsWith(".jar")) || entry.isDirectory()

    def fileToArtifact(f: Attributed[PluginCompat.FileRef]): ModuleArtifact = {
      log.debug("loading artifact for path: " + f)
      ModuleArtifact(
        artifactLoader.load(PluginCompat.toFile(f.data)),
        f.get(PluginCompat.moduleIDStr).map(PluginCompat.parseModuleIDStrAttribute)
      )
    }

    cp.filter(c => isValid(PluginCompat.toFile(c.data))).map(fileToArtifact).toList
  }

  private def filterConflicts[T <: PackageFilter](
      packageFilters: Seq[T],
      setting: SettingKey[?],
      log: Logger,
      name: String,
      field: Dependency => ClassTypeDescriptor,
  )(implicit pfs: PackageFilters[T]): Seq[Conflict] => Seq[Conflict] = { input =>
    if (packageFilters.nonEmpty) {
      log.debug(s"Applying filters on $name packages: ${packageFilters.mkString(", ")}")

      def isFiltered(conflict: Conflict): Boolean = {
        val descriptor = field(conflict.dependency())
        val className = descriptor.getClassName.replace('/', '.')
        val conflictPackageName = className.substring(0, className.lastIndexOf('.'))

        pfs.apply(conflictPackageName)(packageFilters)
      }

      val filtered = input.filter(isFiltered)
      val diff = input.length - filtered.length

      if (diff != 0) {
        log.info(
          s"""
            |$diff conflicts found in ignored ${name} packages.
            |Run plugin again without the '${setting.key.label}' setting to see all conflicts that were found.
             """.stripMargin
        )
      }

      filtered
    } else {
      input
    }
  }

  private def outputConflicts(conflicts: Seq[Conflict], log: Logger): Unit = {
    def logLine(msg: String): Unit =
      log.error(msg)

    val descriptions = Map(
      ConflictCategory.CLASS_NOT_FOUND -> "Class being called not found",
      ConflictCategory.METHOD_SIGNATURE_NOT_FOUND -> "Method being called not found",
    )

    // group conflict by category
    val byCategory = conflicts.groupBy(_.category())

    for ((category, conflictsInCategory) <- byCategory) {
      val desc = descriptions.getOrElse(category, category.name().replace('_', ' '))
      logLine("")
      logLine("Category: " + desc)

      // next group by artifact containing the conflict
      val byArtifact = conflictsInCategory.groupBy(_.usedBy())

      for ((artifactName, conflictsInArtifact) <- byArtifact) {
        logLine("  In artifact: " + artifactName.name())

        // next group by class containing the conflict
        val byClassName = conflictsInArtifact.groupBy(_.dependency().fromClass())

        for ((classDesc, conflictsInClass) <- byClassName) {
          logLine("    In class: " + classDesc.toString())

          for (conflict <- conflictsInClass) {
            def optionalLineNumber(lineNumber: Int): String =
              if (lineNumber != 0) ":" + lineNumber else ""

            val dep = conflict.dependency()
            logLine(
              "      In method:  " +
                dep.fromMethod().prettyWithoutReturnType() +
                optionalLineNumber(dep.fromLineNumber())
            )
            logLine("      " + dep.describe())
            logLine("      Problem: " + conflict.reason())
            if (conflict.existsIn() != ConflictChecker.UNKNOWN_ARTIFACT_NAME)
              logLine("      Found in: " + conflict.existsIn().name())
            // this could be smarter about separating each blob of warnings by method, but for
            // now just output a bunch of dashes always
            logLine("      --------")
          }
        }
      }
    }
  }

  /** Used by ScalaLibraryPlugin to apply custom conflict filters. */
  def checkConflicts(
    cp: Classpath,
    classDirectory: File,
    scanDependencies: Boolean,
    excluded: ModuleFilter,
    log: Logger
  )(implicit converter: FileConverter): Seq[Conflict] =
    loadArtifactsAndCheckConflicts(cp, classDirectory, scanDependencies, excluded, log)

  def logConflicts(conflicts: Seq[Conflict], log: Logger): Unit =
    outputConflicts(conflicts, log)

  private final case class ModuleArtifact(artifact: Artifact, module: Option[ModuleID])

  private[sbtmissinglink] sealed trait PackageFilter {
    def apply(name: String): Boolean
  }
  private[sbtmissinglink] trait PackageFilters[T <: PackageFilter] {
    def apply(name: String)(filters: Seq[T]): Boolean
  }
}
