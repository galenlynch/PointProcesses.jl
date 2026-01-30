__precompile__()
module PointProcesses

import Base:
    count, show, size, getindex, setindex!, IndexStyle, isless, in

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
    midpoint,
    interval_intersections,
    maximum_interval_overlap

using DataStructures: DataStructures, BinaryMinHeap, top
using GLUtilities: GLUtilities, clipsize!, intervals_are_ordered, n_ndx, overlap_interval_union
using Statistics: Statistics, mean

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
    points_intersects,
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
    join_points,
    chunk,
    shrink,
    shift_interval,
    subinterval,
    mask_events,
    midpoint,
    interval_intersections,
    interval_intersections_subpoints,
    nakedinterval,
    nakedvalues,
    nakedpointvalue,
    nakedpoints,
    translate

include("point.jl")
include("intervals.jl")
include("points.jl")

end # module
