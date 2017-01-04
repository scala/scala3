package dotty.tools
package dottydoc
package model
package comment

case class Comment (
  body:                    String,
  short:                   String,
  authors:                 List[String],
  see:                     List[String],
  result:                  Option[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 Option[String],
  since:                   Option[String],
  todo:                    List[String],
  deprecated:              Option[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             Option[String],
  group:                   Option[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  /** List of conversions to hide - containing e.g: `scala.Predef.FloatArrayOps` */
  hideImplicitConversions: List[String]
)

private[comment] case class ParsedComment (
  body:                    String,
  authors:                 List[String],
  see:                     List[String],
  result:                  List[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 List[String],
  since:                   List[String],
  todo:                    List[String],
  deprecated:              List[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             List[String],
  group:                   List[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  hideImplicitConversions: List[String],
  shortDescription:        List[String]
)
