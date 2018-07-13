module PointProcesses

import Base: count
import GLUtilities: duration, time_interval

using GLUtilities

export
    # Types
    Point,
    PointProcess,
    NakedPoint,
    NakedPointProcess,
    MarkedPoint,
    MarkedPointProcess,
    VariablePointProcess,
    SubPointProcess,

    # Functions
    duration,
    points,
    point_values,
    pp_downsamp,
    time_interval

include("points.jl")
include("pointprocesses.jl")

end # module
