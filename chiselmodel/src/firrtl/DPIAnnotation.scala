package chiselmodel.firrtl

import firrtl.annotations._

/** DPIAnnotation to store data to be exposed to DPI. */
case class DPIAnnotation(
  data:      ReferenceTarget,
  condition: ReferenceTarget,
  clock:     ReferenceTarget,
  reset:     ReferenceTarget)
    extends MultiTargetAnnotation {
  require(
    Set(data.moduleTarget, condition.moduleTarget, clock.moduleTarget).size == 1,
    "data, condition, clock should in the same Module."
  )

  val module: ModuleTarget = data.moduleTarget

  override def targets: Seq[Seq[ReferenceTarget]] = Seq(Seq(data), Seq(condition), Seq(clock), Seq(reset))

  override def duplicate(n: Seq[Seq[Target]]): Annotation = this.copy(
    n(0).head.asInstanceOf[ReferenceTarget],
    n(1).head.asInstanceOf[ReferenceTarget],
    n(2).head.asInstanceOf[ReferenceTarget],
    n(3).head.asInstanceOf[ReferenceTarget]
  )
}
