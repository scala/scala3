package dotty.dokka.tasty

trait SyntheticsSupport:
  self: TastyParser =>

  import reflect._

  def isValidPos(pos: Position) = 
    val a = (pos.exists, pos.start, pos.end)
    pos.exists && pos.start != pos.end

  def constructorWithoutParamLists(c: ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.typeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.typeParams.nonEmpty && end <= typesEnd + 1
    }