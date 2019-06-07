package scala.tasty.reflect

trait RootPosition extends Core {

  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position = kernel.rootPosition

}
