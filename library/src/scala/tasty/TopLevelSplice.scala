package scala.tasty

/** Context in a top level ~ at inline site, intrinsically as `import TopLevelSplice._` */
object TopLevelSplice {
  /** Compiler tasty context available in a top level ~ at inline site */
  implicit def tastyContext: Tasty = throw new Exception("Not in macro.")
}
