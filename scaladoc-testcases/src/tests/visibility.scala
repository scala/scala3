package tests
package visibility

import scala.language.`3.3` // to avoid migration warnings/errors of private[this]

private object PrivateTopLevelObject //unexpected

private[tests] object PrivateInOuterPackageTopLevelObject //unexpected

private[visibility] object PrivateInInnerPackageTopLevelObject //unexpected

private[this] object LocallyPrivateTopLevelObject //unexpected

protected object ProtectedTopLevelObject //unexpected

protected[tests] object ProtectedInOuterPackageTopLevelObject //unexpected

protected[visibility] object ProtectedInInnerPackageTopLevelObject //unexpected

protected[this] object LocallyProtectedTopLevelObject //unexpected

private def privateTopLevelMethod: Int //unexpected
    = 1

protected def protectedTopLevelMethod: Int //unexpected
    = 1

class InClassVisibility()
{
    private def privateMethod: Int //unexpected
      = ???

    private[tests] def privateInOuterPackageMethod: Int //unexpected
      = ???

    private[visibility] def privateInInnerPackageMethod: Int //unexpected
      = ???

    private[InClassVisibility] def privateInClassMethod: Int //unexpected
      = ???

    private[this] def locallyPrivateMethod: Int //unexpected
      = ???

    protected def protectedMethod: Int
      = ???

    protected[tests] def protectedInOuterPackageMethod: Int //unexpected
      = ???

    protected[visibility] def protectedInInnerPackageMethod: Int //unexpected
      = ???

    protected[InClassVisibility] def protectedInClassMethod: Int
      = ???

    protected[this] def locallyProtectedMethod: Int //unexpected
      = ???
}

trait InTraitVisibility
{
  protected[InTraitVisibility] def protectedInTraitMethod: Int
    = ???
}

object InObjectVisibility
{
  protected def protectedObjectMethod: Int //unexpected
    = ???

  protected[InObjectVisibility] def protectedInObjectScopeMethod: Int //unexpected
    = ???
}
