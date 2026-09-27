import java.net.URLClassLoader

lazy val root = project.in(file(".")).
  settings(
    TaskKey[Unit]("check-first") := checkTask("First").value,
    TaskKey[Unit]("check-second") := checkTask("Second").value
  )

def checkTask(className: String) = Def.task {
  val converter = fileConverter.value
  val runClasspath = (Runtime / fullClasspath).value
  val cp = runClasspath.map(entry => converter.toPath(entry.data).toFile.toURI.toURL).toArray
  Class.forName(className, false, new URLClassLoader(cp))
  ()
}
