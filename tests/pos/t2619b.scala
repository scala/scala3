abstract class AbstractModule

object ModuleBE extends AbstractModule
object ModuleBF extends AbstractModule

object ModuleBM extends AbstractModule {
    def ms: List[AbstractModule] = List(ModuleBE) ::: List(ModuleBF)
}
