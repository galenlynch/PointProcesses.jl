__precompile__()
module PointProcesses

import Base: count
import GLUtilities: duration, time_interval

using Compat, GLUtilities

@static if VERSION >= v"0.7.0-DEV.2575"
    using Statistics
end

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
    pt_merge,
    pt_extent_merge

include("point.jl")
include("points.jl")

end # module
