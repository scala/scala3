import scala.deriving.Mirror

trait MixinAMini {
  lazy val mixinB = new MixinBMini() {}
}
trait MixinBMini {
  sealed trait Lst // crucially, no companion is defined
  case class Cn(h: Int, t: Lst) extends Lst
  case object Nl extends Lst
}
trait SubABMini extends MixinAMini with MixinBMini {
  val mirror_SubABMini_super_mixinB_Lst =
    summon[Mirror.Of[SubABMini.super[MixinAMini].mixinB.Lst]]
}
