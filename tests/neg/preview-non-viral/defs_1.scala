//> using options -preview
package scala // @preview is private[scala]
import scala.annotation.internal.preview

@preview def previewFeature = 42

def usePreviewFeature = previewFeature
