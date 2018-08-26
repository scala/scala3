import annotation.unchecked.uncheckedVariance

class InvClass[U]
trait InvTrait[U]
class ContraClass[-T]
trait ContraTrait[-T]
class CoClass[+T]
trait CoTrait[+T]

trait Prefix1 {
  trait Prefix2 {
    class InvClass[U]
    trait InvTrait[U]
    class ContraClass[-T]
    trait ContraTrait[-T]
    class CoClass[+T]
    trait CoTrait[+T]
  }
}

class Contra[-T] extends InvClass[T] @uncheckedVariance
class Co[+T] extends InvClass[T] @uncheckedVariance

class ContraInvT[-T] extends InvTrait[T] @uncheckedVariance
class ContraCoT[-T] extends CoTrait[T] @uncheckedVariance
class CoInvT[+T] extends InvTrait[T] @uncheckedVariance
class CoContraT[+T] extends ContraTrait[T] @uncheckedVariance
class CoInvCoT[+T] extends InvTrait[T] @uncheckedVariance with CoTrait[T]
class CoContraCoT[+T] extends ContraTrait[T] @uncheckedVariance with CoTrait[T]

class ContraContraInv[-T] extends ContraClass[T] with InvTrait[T] @uncheckedVariance
class ContraCoInv[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance
class CoCoInv[+T] extends CoClass[T] with InvTrait[T] @uncheckedVariance

class ContraInvContra[-T] extends InvClass[T] @uncheckedVariance with ContraTrait[T]
class CoInvContra[+T] extends InvClass[T] @uncheckedVariance with ContraTrait[T] @uncheckedVariance
class ContraInvCo[-T] extends InvClass[T] @uncheckedVariance with CoTrait[T] @uncheckedVariance
class CoInvCo[+T] extends InvClass[T] @uncheckedVariance with CoTrait[T]

// @uncheckedVariance but also in the wrong place
class CoInvCoT2[+T] extends InvTrait[T] @uncheckedVariance with CoTrait[T] @uncheckedVariance
class ContraContraInv2[-T] extends ContraClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance
class ContraCoInv2[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance

trait Prefix3 {
  def f(v1: Prefix1)(v2: v1.Prefix2): Unit = {
    class Contra[-T] extends InvClass[T] @uncheckedVariance
    class Co[+T] extends InvClass[T] @uncheckedVariance

    class ContraInvT[-T] extends InvTrait[T] @uncheckedVariance
    class ContraCoT[-T] extends CoTrait[T] @uncheckedVariance
    class CoInvT[+T] extends InvTrait[T] @uncheckedVariance
    class CoContraT[+T] extends ContraTrait[T] @uncheckedVariance
    class CoInvCoT[+T] extends InvTrait[T] @uncheckedVariance with CoTrait[T]
    class CoContraCoT[+T] extends ContraTrait[T] @uncheckedVariance with CoTrait[T]

    class ContraContraInv[-T] extends ContraClass[T] with InvTrait[T] @uncheckedVariance
    class ContraCoInv[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance
    class CoCoInv[+T] extends CoClass[T] with InvTrait[T] @uncheckedVariance

    class ContraInvContra[-T] extends InvClass[T] @uncheckedVariance with ContraTrait[T]
    class CoInvContra[+T] extends InvClass[T] @uncheckedVariance with ContraTrait[T] @uncheckedVariance
    class ContraInvCo[-T] extends InvClass[T] @uncheckedVariance with CoTrait[T] @uncheckedVariance
    class CoInvCo[+T] extends InvClass[T] @uncheckedVariance with CoTrait[T]

    // @uncheckedVariance but also in the wrong place
    class CoInvCoT2[+T] extends InvTrait[T] @uncheckedVariance with CoTrait[T] @uncheckedVariance
    class ContraContraInv2[-T] extends ContraClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance
    class ContraCoInv2[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] @uncheckedVariance
  }
}
