package dotty.tools.dotc.tastyreflect

trait SignatureOpsImpl extends scala.tasty.reflect.SignatureOps with CoreImpl {

  object Signature extends SignatureModule {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)] = {
      Some((x.paramsSig.map(_.toString), x.resSig.toString))
    }
  }

}
