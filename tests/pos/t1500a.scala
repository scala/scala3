trait Step0
trait Step1
trait Step2
trait Step3
trait Step4
trait Step5
trait Step6

object Steps {
  implicit val Step0: Step0 = new Step0 {}
  implicit def Step1(implicit p: Step0): Step1 = new Step1 {}
  implicit def Step2(implicit p: Step1): Step2 = new Step2 {}
  implicit def Step3(implicit p: Step2): Step3 = new Step3 {}
  implicit def Step4(implicit p: Step3): Step4 = new Step4 {}
  implicit def Step5(implicit p: Step4): Step5 = new Step5 {}
  implicit def Step6(implicit p: Step5): Step6 = new Step6 {}
}

object StepsTest {
  import Steps.*

  implicitly[Step0]
  implicitly[Step1]
  implicitly[Step2]
  implicitly[Step3]
  implicitly[Step4]
  implicitly[Step6]
}
