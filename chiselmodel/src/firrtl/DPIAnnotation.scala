package chiselmodel.firrtl

import firrtl.RenameMap
import firrtl.annotations._

/** DPIAnnotation to store data to be exposed to DPI. */
case class DPIAnnotation(
  data:      ReferenceTarget,
  condition: ReferenceTarget,
  clock:     ReferenceTarget,
  reset:     ReferenceTarget,
  name:      String)
    extends MultiTargetAnnotation {
  require(
    Set(data.moduleTarget, condition.moduleTarget, clock.moduleTarget).size == 1,
    "data, condition, clock should in the same Module."
  )

  val module: ModuleTarget = data.moduleTarget

  override def targets: Seq[Seq[ReferenceTarget]] = Seq(Seq(data), Seq(condition), Seq(clock), Seq(reset))

  def duplicates(n: Seq[Seq[Target]]): Annotation = {
    this.copy(
      n(0).head.asInstanceOf[ReferenceTarget],
      n(1).head.asInstanceOf[ReferenceTarget],
      n(2).head.asInstanceOf[ReferenceTarget],
      n(3).head.asInstanceOf[ReferenceTarget]
    )
  }

  override def update(renames: RenameMap): Seq[Annotation] =
    crossJoin(targets.map(ts => ts.flatMap(renames(_)))).map(newTargets =>
      DPIAnnotation(
        newTargets(0).asInstanceOf[ReferenceTarget],
        newTargets(1).asInstanceOf[ReferenceTarget],
        newTargets(2).asInstanceOf[ReferenceTarget],
        newTargets(3).asInstanceOf[ReferenceTarget],
        name
      )
    )

  private def crossJoin[T](list: Seq[Seq[T]]): Seq[Seq[T]] =
    list match {
      case Nil      => Nil
      case x :: Nil => x.map(Seq(_))
      case x :: xs =>
        val xsJoin = crossJoin(xs)
        for {
          i <- x
          j <- xsJoin
        } yield {
          Seq(i) ++ j
        }
    }

  /** dummy not used */
  override def duplicate(n: Seq[Seq[Target]]): Annotation = {
    require(false, "this should not being called")
    this
  }
}
