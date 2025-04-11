//> using options -Yimplicit-as-given

object Lib:
  class LibComponent:
    def msg = "found if given"
  implicit val libComponent: LibComponent = LibComponent()

  class UserComponent extends LibComponent:
      override def msg = "found if implicit"
  implicit val userComponent: UserComponent = UserComponent()

  def printComponent(implicit c: LibComponent) = println(c.msg)


@main def Test = Lib.printComponent
