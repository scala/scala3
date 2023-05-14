package dotty.tools.scaladoc.util

object Check:
  /**  
   *  Jekyll (also used by GitHub Pages) by default makes a couple characters 
   *  illegal to use in file name beginnings.
   */
  def checkJekyllIncompatPath(path: Seq[String]): Boolean =
    path.find( filename =>
      filename.matches("^~.*")
       || filename.matches("^\\_.*")
       || filename.matches("^\\..*")
       || filename.matches("^\\#.*")
    ).isDefined