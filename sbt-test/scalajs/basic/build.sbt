lazy val testIRPositions = taskKey[Unit]("test IR positions (#14240)")

enablePlugins(ScalaJSPlugin)

scalaVersion := sys.props("plugin.scalaVersion")

// Test withDottyCompat for %%% dependencies
libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(CrossVersion.for3Use2_13)

scalaJSUseMainModuleInitializer := true

// #14240 Make sure that generated IR positions are 0-based
testIRPositions := {
  import scala.concurrent.{Future, _}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.util.{Failure, Success}

  import org.scalajs.ir.Names._
  import org.scalajs.ir.Position
  import org.scalajs.ir.Trees._
  import org.scalajs.linker.interface.unstable.IRFileImpl

  val ir = (Compile / scalaJSIR).value
  val classNameToTest = ClassName("test.Main$")

  val classDefFuture = {
    // This logic is copied from the implementation of `scalajsp` in sbt-scalajs
    Future.traverse(ir.data) { irFile =>
      val ir = IRFileImpl.fromIRFile(irFile)
      ir.entryPointsInfo.map { i =>
        if (i.className == classNameToTest) Success(Some(ir))
        else Success(None)
      }.recover { case t => Failure(t) }
    }.flatMap { irs =>
      irs.collectFirst {
        case Success(Some(f)) => f.tree
      }.getOrElse {
        val t = new MessageOnlyException(s"class ${classNameToTest.nameString} not found on classpath")
        irs.collect { case Failure(st) => t.addSuppressed(st) }
        throw t
      }
    }
  }
  val classDef = Await.result(classDefFuture, Duration.Inf)

  def testPos(pos: Position, expectedLine: Int, expectedColumn: Int): Unit = {
    if (!pos.source.getPath.endsWith("/Main.scala") || pos.line != expectedLine || pos.column != expectedColumn)
      throw new MessageOnlyException(s"Expected Main.scala@$expectedLine:$expectedColumn but got $pos")
  }

  testPos(classDef.pos, 5, 7)
}
