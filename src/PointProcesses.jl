__precompile__()
module PointProcesses

import Base: count
import GLUtilities:
    duration,
    time_interval,
    measure,
    is_subinterval,
    check_overlap,
    interval_indices,
    interval_intersect,
    mask_events,
    intervals_diff,
    midpoint

using Compat, GLUtilities, DataStructures

@static if VERSION >= v"0.7.0-DEV.2575"
    using Statistics
end

export
    # Types
    Point,
    Interval,
    NakedInterval,
    MarkedInterval,
    RelativeInterval,
    IntervalSet,
    Points,
    NakedPoint,
    NakedPoints,
    MarkedPoint,
    MarkedPoints,
    VariablePoints,
    SubPoints,

    # Functions
    duration,
    rate,
    maybe_subpoints,
    points,
    point_values,
    pp_downsamp,
    time_interval,
    push_mark,
    pop_mark,
    pop_marks,
    pt_merge,
    pt_extent_merge,
    bounds,
    measure,
    get_mark,
    interval,
    complement,
    interval_levels,
    chunk,
    shrink,
    subinterval,
    mask_events,
    midpoint

include("point.jl")
include("intervals.jl")
include("points.jl")

end # module
