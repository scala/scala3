package dotty.tools.dotc.plugins

import scala.language.unsafeNulls

import org.junit.Test

import dotty.tools.dotc._
import plugins._
import transform.MegaPhase.MiniPhase
import core.Phases.Phase

class PluginsTest {
  class TestPhase extends PluginPhase { def phaseName = this.getClass.getName }
  class P1  extends TestPhase
  class P2  extends TestPhase
  class P3a extends TestPhase
  class P3b extends TestPhase
  class P3c extends TestPhase
  class P3d extends TestPhase
  class P3e extends TestPhase
  class P4  extends TestPhase
  class P5  extends TestPhase
  class P6a extends TestPhase
  class P6b extends TestPhase
  class P6c extends TestPhase
  class P6d extends TestPhase
  class P6e extends TestPhase
  class P7  extends TestPhase
  class P8  extends TestPhase

  val basicPlan = List(
    List(new P1),
    List(new P2),
    List(new P3a, new P3b, new P3c, new P3d, new P3e),
    List(new P4),
    List(new P5),
    List(new P6a, new P6b, new P6c, new P6d, new P6e),
    List(new P7),
    List(new P8)
  )

  implicit def clazzToName(cls: Class[?]): String = cls.getName

  def debugPlan(plan: List[List[Phase]]): Unit = {
    println(plan.mkString("plan:\n- ", "\n- ", ""))
  }

  @Test
  def insertAfter = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
    }

    val updatedPlan = Plugins.schedule(basicPlan, M1 :: Nil)
    assert(updatedPlan(3)(0) eq M1)
  }

  @Test
  def insertBefore = {
    object ConstFold extends TestPhase {
      override val runsBefore = Set(classOf[P7])
    }

    val updatedPlan = Plugins.schedule(basicPlan, ConstFold :: Nil)
    assert(updatedPlan(6)(0) eq ConstFold)
  }

  @Test
  def insertBeforeAfter = {
    object ConstFold extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }

    // prefers the runsBefore
    val updatedPlan = Plugins.schedule(basicPlan, ConstFold :: Nil)
    assert(updatedPlan(6)(0) eq ConstFold)
  }

  @Test
  def constraintUnsatisfiable = {
    object ConstFold extends TestPhase {
      override val runsAfter = Set(classOf[P6d])
      override val runsBefore = Set(classOf[P2], classOf[P8])
    }

    try {
      Plugins.schedule(basicPlan, ConstFold :: Nil)
      assert(false, "unsatisfiable constraint should throw exception, but not")
    } catch {
      case _: Exception =>
    }
  }

  @Test
  def orderingTwoPlugins1 = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(M2.phaseName, classOf[P7], classOf[P8])
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }

    // M1 inserted to plan first
    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: Nil)
    assert(updatedPlan1(6)(0) eq M1)
    assert(updatedPlan1(7)(0) eq M2)

    // M2 inserted to plan first
    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: Nil)
    assert(updatedPlan2(6)(0) eq M1)
    assert(updatedPlan2(7)(0) eq M2)
  }

  @Test
  def orderingTwoPlugins2 = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d], M2.phaseName)
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }

    // M1 inserted to plan first
    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: Nil)
    assert(updatedPlan1(4)(0) eq M1)
    assert(updatedPlan1(3)(0) eq M2)

    // M2 inserted to plan first
    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: Nil)
    assert(updatedPlan2(4)(0) eq M1)
    assert(updatedPlan2(3)(0) eq M2)
  }

  @Test
  def orderingTwoPlugins3 = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d], M2.phaseName)
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P5])
    }

    // M1 inserted to plan first
    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: Nil)
    assert(updatedPlan1(7)(0) eq M1)
    assert(updatedPlan1(4)(0) eq M2)

    // M2 inserted to plan first
    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: Nil)
    assert(updatedPlan2(7)(0) eq M1)
    assert(updatedPlan2(4)(0) eq M2)
  }

  @Test
  def orderingTwoPlugins4 = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(M2.phaseName, classOf[P7])
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P5], classOf[P8])
    }

    // M1 inserted to plan first
    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: Nil)
    assert(updatedPlan1(4)(0) eq M1)
    assert(updatedPlan1(5)(0) eq M2)

    // M2 inserted to plan first
    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: Nil)
    assert(updatedPlan2(4)(0) eq M1)
    assert(updatedPlan2(5)(0) eq M2)
  }

  @Test
  def orderingTransitive = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(M2.phaseName, classOf[P7])
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P5], classOf[P8])
    }
    object M3 extends TestPhase {
      override val runsAfter = Set(M2.phaseName, classOf[P2])
      override val runsBefore = Set(classOf[P4], classOf[P8])
    }

    // M1 inserted to plan first
    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: M3 :: Nil)
    assert(updatedPlan1(3)(0) eq M1)
    assert(updatedPlan1(4)(0) eq M2)
    assert(updatedPlan1(5)(0) eq M3)

    // M2 inserted to plan first
    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: M3 :: Nil)
    assert(updatedPlan1(3)(0) eq M1)
    assert(updatedPlan1(4)(0) eq M2)
    assert(updatedPlan1(5)(0) eq M3)
  }


  @Test
  def deterministic = {
    object M1 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }
    object M2 extends TestPhase {
      override val runsAfter = Set(classOf[P3d])
      override val runsBefore = Set(classOf[P7], classOf[P8])
    }

    val updatedPlan1 = Plugins.schedule(basicPlan, M1 :: M2 :: Nil)
    assert(updatedPlan1(6)(0) eq M1)
    assert(updatedPlan1(7)(0) eq M2)

    val updatedPlan2 = Plugins.schedule(basicPlan, M2 :: M1 :: Nil)
    assert(updatedPlan1(6)(0) eq M1)
    assert(updatedPlan1(7)(0) eq M2)
  }
}
