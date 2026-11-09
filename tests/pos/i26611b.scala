import scala.deriving.Mirror

case class Local(y: Int)
object Local

// `| Any` is for make typer pass
// Run Local <:< Mirror.Product check at typer (baseTypeCache for `Mirror.Product` on `Local$` <- NoType)
// PostTyper adds `MirrorPrpduct` as a parent of `Local$`, we should invalidate baseTypeCache for `Local$`
// otherwise, Recheck fails `Local$ <:< Mirror.Product`.
val x: Mirror.Product | Any = Local
