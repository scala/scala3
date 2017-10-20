import java.net.URLClassLoader

lazy val root = project.in(file(".")).
  settings(
    TaskKey[Unit]("check-first") := checkTask("First").value,
    TaskKey[Unit]("check-second") := checkTask("Second").value
  )

def checkTask(className: String) = Def.task {
  val runClasspath = (fullClasspath in Runtime).value
  val cp = runClasspath.map(_.data.toURI.toURL).toArray
  Class.forName(className, false, new URLClassLoader(cp))
  ()
}
