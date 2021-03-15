import sbt._
import Keys._

/* With <3 from scala-js */
object NoBloopExport {
  private lazy val bloopGenerateKey: Option[TaskKey[Option[File]]] = {
    val optBloopKeysClass: Option[Class[_]] = try {
      Some(Class.forName("bloop.integrations.sbt.BloopKeys"))
    } catch {
      case _: ClassNotFoundException => None
    }

    optBloopKeysClass.map { bloopKeysClass =>
      val bloopGenerateGetter = bloopKeysClass.getMethod("bloopGenerate")
      bloopGenerateGetter.invoke(null).asInstanceOf[TaskKey[Option[File]]]
    }
  }

  /** Settings to prevent the project from being exported to IDEs. */
  lazy val settings: Seq[Setting[_]] = {
    bloopGenerateKey match {
      case None =>
        Nil
      case Some(key) =>
        Seq(
            Compile / key := None,
            Test / key := None,
        )
    }
  }
}
