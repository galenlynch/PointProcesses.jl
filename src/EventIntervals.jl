"""
    EventIntervals

Interval-aware data structures for point processes. Couples timestamped event
collections ([`Points`](@ref)) with the temporal domains ([`Interval`](@ref)) they are
defined on, so that slicing, intersecting, downsampling, and transforming operations keep
points and their intervals in sync.

Core types come in "naked" (coordinates only) and "marked" (coordinates + metadata)
variants: [`NakedPoint`](@ref)/[`MarkedPoint`](@ref),
[`NakedInterval`](@ref)/[`MarkedInterval`](@ref),
[`NakedPoints`](@ref)/[`VariablePoints`](@ref). [`SubPoints`](@ref) provides lazy,
zero-copy views into any points collection.
"""
module EventIntervals

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
    midpoint,
    relative_interval

import SignalIndices:
    time_interval,
    duration

using DataStructures: DataStructures, BinaryMinHeap

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
    interval_intersect,
    interval_intersections,
    interval_intersections_subpoints,
    interval_indices,
    intervals_diff,
    is_subinterval,
    check_overlap,
    maximum_interval_overlap,
    nakedinterval,
    nakedvalues,
    nakedpointvalue,
    nakedpoints,
    translate,
    relative_interval

include("point.jl")
include("intervals.jl")
include("points.jl")

end # module
