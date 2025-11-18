//> using options -Yexplicit-nulls -Werror

trait AwtComponentLogging extends java.awt.Component:

  override def getPreferredSize: java.awt.Dimension =
    val dim: java.awt.Dimension = super.getPreferredSize
    dim

  private def superPS1 = super.getPreferredSize

  private val superPS2 = super.getPreferredSize
