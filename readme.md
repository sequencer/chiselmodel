# ChiselModel

## Introduction
This is a experimental library interacting Chisel based design to foreign language with DPI, to inject arbitrary software model to Chisel and simulate together.

## Basic Idea
### Frontend code generation
1. You can execute `dpi(data: Data)` to capture a DPI executing environment(`clock`, `reset`, `condition`).
2. A `dpi` function all will gather current `clock`, `reset`, `condition`, `data` annotate them to `DPIAnnotation`.
3. In FIRRTL, `DPITransform` will gather all `DPIAnnotations` constructing a SystemVerilog blackbox with CIRCT, connecting all signals to the CIRCT blackbox.

### Backend model generation
1. Model can be implemented with different languages, like Rust, Scala, Python.
2. Basic idea is constructing a RPC as an interaction between SystemVerilog.
3. Advanced idea is using open array avoid data copy to speed up simulation.

## Road Map
[ ] Get Chisel#1694 merged.
[x] Implement dpi related annotation and API in Chisel side.
[ ] Implement dpi related transform on FIRRTL side.
[ ] Construct a basic SystemVerilog String template.
[ ] Get GCD be able to work, test C model and Chisel model behave same.
[ ] Switch to CIRCT blackbox generation.
[ ] Switch to CIRCT cosim library for a standardized RPC API.
[ ] Add Open Array avoid data copy between simulator and model.
