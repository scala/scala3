package mdscala

/** Contains function to generate markdown (follows CommonMarkdown specifications)*/
object Md {
  /** Form a header
   *
   *  @param obj Label of the header, usually a String
   *  @param level Header level, 1-6
   *  @return The formed header as a String
   */
  def header(obj: Any, level: Int) : String = {
    require(level <= 6 && level >= 1, "Wrong header level")
    "#" * level + " " + obj.toString + "\n"
  }

  /** Form a header of level 1
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header1(obj: Any) : String = {
    header(obj, 1)
  }

  /** Form a header of level 2
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header2(obj: Any) : String = {
    header(obj, 2)
  }

  /** Form a header of level 3
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header3(obj: Any) : String = {
    header(obj, 3)
  }

  /** Form a header of level 4
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header4(obj: Any) : String = {
    header(obj, 4)
  }

  /** Form a header of level 5
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header5(obj: Any) : String = {
    header(obj, 5)
  }

  /** Form a header of level 6
   *
   *  @param obj Label of the header, usually a String
   *  @return The formed header as a String
   */
  def header6(obj: Any) : String = {
    header(obj, 6)
  }

  /** Form a fenced code block
   *
   *  @param obj The content of the code block
   *  @param language Specific language for the syntax highlight (default: no language)
   *  @return The formed code block
   */
  def codeBlock(obj: Any, language : String = "") : String = {
    "```" + language + "\n" + obj.toString + "\n```\n"
  }
}