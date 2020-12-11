package chiselmodel.firrtl

import firrtl.annotations.{ReferenceTarget, TargetToken}
import firrtl.passes.PassException

class DPIAnnotatedToNoPortTargetException(data: ReferenceTarget)
    extends PassException(
      s"[module ${data.module}] annotated target ${data.ref} is not a Port, DPI can not be annotated to Port."
    )

class DPINameCollapsedException(pathGroup: Map[Seq[(TargetToken.Instance, TargetToken.OfModule)], Seq[DPIAnnotation]])
    extends PassException(
      s"DPI Name collapsed detected:\n${pathGroup.map { case (key, value) => s"${key.mkString("_")} -> $value\n" }}"
    )

class DPIClockTypeException(clock: ReferenceTarget)
    extends PassException(s"type of ${clock.serialize} can only be Clock.")

class DPIResetTypeException(reset: ReferenceTarget)
    extends PassException(s"type of ${reset.serialize} can only be UInt<1> or AsyncResetType.")

class DPIConditionTypeException(condition: ReferenceTarget)
    extends PassException(s"type of ${condition.serialize} can only be UInt<1>.")
