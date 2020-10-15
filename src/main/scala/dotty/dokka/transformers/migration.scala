// package dotty.dokka.transformers

// import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
// import org.jetbrains.dokka.model._
// import collection.JavaConverters._
// import org.jetbrains.dokka.plugability.DokkaContext
// import org.jetbrains.dokka.links.DRI
// import org.jetbrains.dokka.model.properties._
// import collection.JavaConverters._

// import dotty.dokka.model.api._
// import dotty.dokka.put


// object MigrationUtils:
//   def populateInfo(d: DClass): DClass = 
//     val a: ClasslikeExtension = ???
//     d.put(ClasslikeExtension(
//       d.getProperties.asScala.toList.map(field),
//       Nil,
//       Nil
//     ))


//   def getVisibility(a: WithVisibility): Visibility = a.getVisibility.asScala.head match
//     case ScalaVisibility.

//   def field(d: DProperty): DClass = 
//     Member(
//       d.getDri,
//       d.getName,
//       ???,
//       d.getDocumentation.asScala.headOption.map(_._2),
//       d.getSources.asScala.headOption.map(_._2),
//       DocumentableExtension(
//         d.getVisibility.asScala.headOption.map(_._2)
//       )
//     )
  