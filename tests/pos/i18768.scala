package minimized:
  object Module:
    object Exportee:

      opaque type Id = Long

      def apply(): Id = ???

      extension (e: Id)
        def updated: Id = ???


  object Client:
    export Module.*
    val x = Exportee().updated

package original:
  object Module:
    trait EntityDef:
      type Id
      type Record
      type Entity = (Id, Record)

      extension (e: Entity)
        def updated: Entity = e

    case class Exportee()
    object Exportee extends EntityDef:
      opaque type Id = Long
      type Record = Exportee

      def apply(id: Long): Entity = (id, Exportee())

  object Client:
    export Module.*
    val x = Exportee(1L).updated


  object ClientWorkingWithManualExport:
    export Module.{Exportee as _, *}
    type Exportee = Module.Exportee
    val Exportee = Module.Exportee

    val x = Exportee(1L).updated
