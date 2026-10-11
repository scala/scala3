package dotty.tools.scaladoc
package signatures

class CaptureCheckingSignatures extends SignatureTest(
  "captureCheckingSignatures",
  SignatureTest.all
)

class CaptureCheckingRendering extends SignatureTest(
  "ccRendering",
  SignatureTest.all.filterNot(_ == "object"),
  sourceFiles = List("ccRendering", "ccRenderingNonCC", "ccRenderingPackageObject", "ccRenderingBase", "ccRenderingLegacy")
)
