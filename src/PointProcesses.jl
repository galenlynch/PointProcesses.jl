__precompile__()
module PointProcesses

import Base:
    count, show, size, getindex, setindex!, IndexStyle, isless, in

import SortedIntervals:
    measure,
    is_subinterval,
    check_overlap,
    interval_indices,
    interval_intersect,
    interval_intersections,
    maximum_interval_overlap,
    mask_events,
    intervals_diff,
    midpoint

import SignalIndices:
    time_interval,
    duration

using DataStructures: DataStructures, BinaryMinHeap, top

using SortedIntervals: clipsize!, intervals_are_ordered, overlap_interval_union
using SignalIndices: n_ndx

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
