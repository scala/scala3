// This one should be rejected according to spec. The import takes precedence
// over the type in the same package because the typeis declared in a
// different compilation unit. scalac does not conform to spec here.
package naming.resolution

import java.nio.file._ // Imports `Files`

object Resolution {
  def gimmeFiles: Files = Files.list(Paths.get("."))
}
