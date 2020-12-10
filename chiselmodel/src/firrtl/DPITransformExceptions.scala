package chiselmodel.firrtl

import firrtl.annotations.ReferenceTarget
import firrtl.passes.PassException

class DPIAnnotatedToNoPortTargetException(data: ReferenceTarget)
    extends PassException(
      s"[module ${data.module}] annotated target ${data.ref} is not a Port, DPI can not be annotated to Port."
    )
