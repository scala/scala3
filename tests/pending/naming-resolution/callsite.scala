package naming.resolution

import java.nio.file._ // Imports `Files`

object Resolution {
  def gimmeFiles: Files = Files.list(Paths.get("."))
}
