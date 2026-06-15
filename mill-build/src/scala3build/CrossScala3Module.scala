package scala3build

import mill.*
import mill.api.Cross.Resolver
import mill.api.Task.Simple

/**
 * Tasks for a bootstrapped module
 *
 * Compiled once with a published compiler, and again with a bootstrapped one
 */
trait CrossScala3Module extends Cross.Module[Mode] with Scala3Module {
  implicit def mode: Mode = crossValue

  def artifactNameParts =
    if (mode == Mode.NonBootstrapped)
      super.artifactNameParts
    else
      Task {
        val baseValue = super.artifactNameParts()
        if (baseValue.endsWith(Seq("bootstrapped"))) baseValue.dropRight(1)
        else baseValue
      }

  def publishDisabled(): Task.Command[Unit] =
    Task.Command[Unit] {
      val org = pomSettings().organization
      val name = artifactId()
      val ver = publishVersion()
      System.err.println(
        s"Not publishing $org:$name:$ver of non-bootstrapped module ${moduleSegments.parts.mkString(".")}"
      )
    }

  override def publishLocal(
    localIvyRepo: String = null,
    sources: Boolean = true,
    doc: Boolean = true,
    transitive: Boolean = false
  ): Task.Command[Unit] =
    if (mode == Mode.NonBootstrapped)
      Task.Command(publishDisabled())
    else
      super.publishLocal(localIvyRepo, sources, doc, transitive)

  override def publishLocalCached: T[Seq[PathRef]] =
    if (mode == Mode.NonBootstrapped)
      Task {
        publishDisabled()
        Nil
      }
    else
      super.publishLocalCached

  override def publishM2Local(m2RepoPath: String): Task.Command[Seq[PathRef]] =
    if (mode == Mode.NonBootstrapped)
      Task.Command {
        publishDisabled()
        Nil
      }
    else
      super.publishM2Local(m2RepoPath)

  override def publishM2LocalCached: T[Seq[PathRef]] =
    if (mode == Mode.NonBootstrapped)
      Task {
        publishDisabled()
        Nil
      }
    else
      super.publishM2LocalCached

  override def publish(
    sonatypeCreds: String,
    signed: Boolean,
    gpgArgs: String,
    release: Boolean,
    readTimeout: Int,
    connectTimeout: Int,
    awaitTimeout: Int,
    stagingRelease: Boolean
  ): Task.Command[Unit] =
    if (mode == Mode.NonBootstrapped)
      Task.Command(publishDisabled())
    else
      super.publish(
        sonatypeCreds,
        signed,
        gpgArgs,
        release,
        readTimeout,
        connectTimeout,
        awaitTimeout,
        stagingRelease
      )

  given Resolver[CrossScala3Module] =
    new Resolver[CrossScala3Module] {
      def resolve[V <: CrossScala3Module](c: Cross[V]): V =
        c.crossModules.find(_.mode == mode).getOrElse {
          throw new Exception(
            s"Cannot find $mode in $c's cross-modules (available modes: ${c.crossModules.map(_.mode).distinct.mkString(", ")})"
          )
        }
    }
}
