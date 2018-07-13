module PointProcesses

import Base: count
import GLUtilities: duration, time_interval

using GLUtilities

export
    # Types
    Point,
    Points,
    NakedPoint,
    NakedPoints,
    MarkedPoint,
    MarkedPoints,
    VariablePoints,
    SubPoints,

    # Functions
    duration,
    points,
    point_values,
    pp_downsamp,
    time_interval,
    push_mark,
    pop_mark,
    pop_marks,
    pt_merge

include("points.jl")
include("pointprocesses.jl")

end # module
