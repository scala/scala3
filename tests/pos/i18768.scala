object Module:
  object Exportee:

    opaque type Id = Long

    def apply(): Id = ???

    extension (e: Id)
      def updated: Id = ???


object Client:
  export Module.*
  val x = Exportee().updated
