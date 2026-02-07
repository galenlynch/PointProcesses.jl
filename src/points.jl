"""
    Points{E,N,I<:Interval{E},T<:Point{E}} <: AbstractVector{T}

Abstract supertype for read-only collections of [`Point`](@ref) values defined on an
[`Interval`](@ref). Subtypes are [`NakedPoints`](@ref), [`VariablePoints`](@ref), and
[`SubPoints`](@ref).

`Points` implements `AbstractVector`, so indexing, iteration, and `length` work as
expected. Assignment via `setindex!` throws `ReadOnlyMemoryError`.
"""
abstract type Points{E,N,I<:Interval{E},T<:Point{E}} <: AbstractVector{T} end

"""
    MarkedPoints{E,N,I,M}

Type alias for `Points{E,N,I,MarkedPoint{E,M}}` — any [`Points`](@ref) collection
whose elements are [`MarkedPoint`](@ref) values.
"""
const MarkedPoints{E,N,I,M} = Points{E,N,I,MarkedPoint{E,M}}

IndexStyle(::Points) = IndexLinear()
setindex!(::Points, v, i...) = throw(ReadOnlyMemoryError())

"""
    rate(p::Points) -> Number
    rate(p::Points, b, e) -> Number
    rate(ps::AbstractVector{<:Points}) -> Number or nothing

Compute the event rate (count / duration). The two-argument form restricts the
calculation to the range `[b, e]`. The vector form computes the aggregate rate across
all collections, returning `nothing` if the vector is empty.
"""
rate(p::Points) = count(p) / duration(p)
rate(p::Points, b, e) = count(p, b, e) / (e - b)
function rate(ps::AbstractVector{<:Points})
    if isempty(ps)
        nothing
    else
        sum(count, ps) / sum(duration, ps)
    end
end

nakedinterval(p::Points) = nakedinterval(interval(p))

"""
    interval(p::Points) -> Interval

Return the domain interval on which the points collection is defined.
"""
interval(p::Points) = interval(nakedpoints(p))

measure(p::Points) = measure(interval(p))

"""
    duration(p::Points) -> Number

Return the duration (measure) of the domain interval. Equivalent to
`measure(interval(p))`.
"""
duration(p::Points) = measure(p)

"""
    time_interval(p::Points) -> Interval

Return the domain interval. Synonym for [`interval`](@ref).
"""
time_interval(p::Points) = interval(p)

bounds(p::Points) = bounds(interval(p))

"""
    points(p::Points, [b, e]) -> Vector{<:Point}

Materialise the [`Point`](@ref) objects from a points collection. The optional range
`[b, e]` restricts to points within those bounds.
"""
function points(p::Points{<:Any,<:Any,<:Any,M}, args...) where {M}
    M.(point_values(p, args...)...)
end

"""
    nakedvalues(p::Points) -> AbstractVector
    nakedvalues(p::Points, b, e) -> AbstractVector

Return the raw timestamp values from a points collection, stripping any marks.
Equivalent to `point_values(nakedpoints(p), ...)`.
"""
nakedvalues(p::Points, b, e) = point_values(nakedpoints(p), b, e)
nakedvalues(p::Points) = nakedvalues(p, bounds(p)...)

"""
    NakedPoints{E,I,A} <: Points{E,1,I,NakedPoint{E}}

A sorted vector of timestamps of type `E` defined on an interval of type `I`.
Points need not be strictly increasing, but must be non-decreasingly sorted.

# Constructors

    NakedPoints(points::AbstractVector{E}, interval::Interval{E})
    NakedPoints(points::AbstractVector{E}, (a, b))
    NakedPoints(points::AbstractVector{E}, a, b)
    NakedPoints(points::AbstractVector)

The first three forms accept an explicit interval; unsorted input is sorted in-place.
The last form infers the interval from `(minimum(points), maximum(points))` and
requires a non-empty vector.

Throws `ArgumentError` if points fall outside the given interval.
"""
struct NakedPoints{E<:Number,I<:Interval{E},A<:AbstractVector{E}} <:
       Points{E,1,I,NakedPoint{E}}
    points::A
    interval::I
    function NakedPoints{E,I,A}(
        points::A,
        interval::I,
    ) where {E,I<:Interval{E},A<:AbstractVector{E}}
        issorted(points) || throw(ArgumentError("Times is not sorted"))
        validate_interval(interval) || throw(ArgumentError("Interval not valid"))
        (b, e) = bounds(interval)
        if ! isempty(points)
            if (points[1] < b) | (points[end] > e)
                throw(ArgumentError("Points exceed interval"))
            end
        end
        new(points, interval)
    end
end
interval(p::NakedPoints) = p.interval
size(p::NakedPoints) = size(p.points)
@inline function getindex(p::NakedPoints, i::Integer)
    @boundscheck checkbounds(Bool, p.points, i) || throw(BoundsError(p, i))
    NakedPoint(@inbounds p.points[i])
end

function NakedPoints(
    points::A,
    interval::I,
    check::Bool = true,
) where {E,I<:Interval{E},A<:AbstractVector{E}}
    if check && ! issorted(points)
        sort!(points)
    end
    NakedPoints{E,I,A}(points, interval)
end

function NakedPoints(
    points::AbstractVector{E},
    interval::NTuple{2,<:Any},
    check::Bool = true,
) where {E}
    NakedPoints(points, NakedInterval(convert(Tuple{E,E}, interval)), check)
end

function NakedPoints(points::AbstractVector{E}, a::Number, b::Number) where {E}
    NakedPoints(points, (convert(E, a), convert(E, b)))
end

function NakedPoints(points::AbstractVector)
    isempty(points) && throw(ArgumentError("points cannot be empty"))
    mypoints = copy(points)
    sort!(mypoints)
    NakedPoints(mypoints, (mypoints[1], mypoints[end]), false)
end

function NakedPoints(points::AbstractVector{<:NakedPoint}, args...)
    NakedPoints(getfield.(points, :point), args...)
end
function NakedPoints(
    points::AbstractVector{<:NakedPoint{E}},
    interval::NTuple{2,<:Any},
    check::Bool = true,
) where {E}
    NakedPoints(getfield.(points, :point), interval, check)
end

validate_interval(int::NTuple{2,Number}) = int[2] >= int[1]
validate_interval(i::Interval) = validate_interval(bounds(i))

function points_show_repl_preamble(io::IO, pts::T) where {T<:Points}
    println(io, length(pts), "–point ", T, " defined on interval ", bounds(pts), ":")
end
function show(io::IO, ::MIME"text/plain", pts::T) where {T<:NakedPoints}
    points_show_repl_preamble(io, pts)
    print(io, "    ", point_values(pts))
end

count(p::NakedPoints) = length(p.points)
function count(p::NakedPoints, range_start, range_end)
    ib, ie = point_range(p, range_start, range_end)
    max(zero(ib), n_ndx(ib, ie))
end

function point_range(p::NakedPoints{E,<:Any,<:Any}, b::E, e::E) where {E}
    b <= e || throw(ArgumentError("beginning and end not well ordered"))
    ib = searchsortedfirst(p.points, b)
    ie = searchsortedlast(p.points, e)
    ib, ie
end
function point_range(p::NakedPoints{E,<:Any,<:Any}, b, e) where {E}
    point_range(p, convert(E, b), convert(E, e))
end


function point_values(p::NakedPoints, b, e)
    ib, ie = point_range(p, b, e)
    view(p.points, ib:ie)
end
point_values(p::NakedPoints) = p.points

points(p::NakedPoints, args...) = NakedPoint.(point_values(p, args...))

"""
    nakedpoints(p::Points) -> NakedPoints

Extract or return the underlying [`NakedPoints`](@ref) (timestamps only, no marks).
Identity on `NakedPoints` inputs. For [`SubPoints`](@ref), materialises a new
`NakedPoints` covering the sub-interval.
"""
nakedpoints(p::NakedPoints) = p

"""
    translate(p::NakedPoints, offset) -> NakedPoints
    translate(p::VariablePoints, offset) -> VariablePoints

Shift all timestamps and the domain interval by `offset`. Marks are preserved.
"""
function translate(p::NakedPoints, offset)
    rb, re = bounds(p)
    NakedPoints(p.points .+ offset, (rb + offset, re + offset))
end

point_range(p::Points, args...) = point_range(nakedpoints(p), args...)
count(p::Points, args...) = count(nakedpoints(p), args...)

"""
    VariablePoints{E,I,P,M,A} <: Points{E,1,I,MarkedPoint{E,M}}

A sorted collection of timestamps paired with per-point marks of type `M`.
Wraps a [`NakedPoints`](@ref) for the timestamps and a separate marks vector.

# Constructors

    VariablePoints(nakedpoints::NakedPoints, marks::AbstractVector)
    VariablePoints(points::AbstractVector, marks::AbstractVector, interval_args...)
    VariablePoints(pts::AbstractVector{<:MarkedPoint}, interval_args...)

The second form sorts `points` and permutes `marks` to match. The third form
destructures `MarkedPoint` elements. All forms require `length(points) == length(marks)`.

See also [`get_mark`](@ref), [`point_values`](@ref).
"""
struct VariablePoints{E,I<:Interval{E},P<:NakedPoints{E,I,<:Any},M,A<:AbstractVector{M}} <:
       Points{E,1,I,MarkedPoint{E,M}}
    nakedpoints::P
    marks::A
    function VariablePoints{E,I,P,M,A}(
        nakedpoints::P,
        marks::A,
    ) where {E,I<:Interval{E},P<:NakedPoints{E},M,A<:AbstractVector{M}}
        if count(nakedpoints) != length(marks)
            throw(DimensionMismatch("point process must be the same size as marks"))
        end
        new(nakedpoints, marks)
    end
end

function VariablePoints(
    nakedpoints::P,
    marks::A,
) where {E,I<:Interval{E},P<:NakedPoints{E,I,<:Any},M,A<:AbstractVector{M}}
    VariablePoints{E,I,P,M,A}(nakedpoints, marks)
end

function VariablePoints(points::AbstractVector, marks::AbstractVector, args...)
    mypoints = copy(points)
    mymarks = copy(marks)
    sp = sortperm(mypoints)
    mypoints = mypoints[sp]
    mymarks = mymarks[sp]
    pp = NakedPoints(mypoints, args...)
    VariablePoints(pp, mymarks)
end

function VariablePoints(pts::AbstractVector{<:MarkedPoint{E,M}}, args...) where {E,M}
    np = length(pts)
    ps = Vector{E}(undef, np)
    mks = Vector{M}(undef, np)
    @inbounds for i = 1:np
        ps[i] = pts[i].point
        mks[i] = pts[i].mark
    end
    VariablePoints(ps, mks, args...)
end

size(p::VariablePoints) = size(nakedpoints(p))

get_mark(p::VariablePoints) = p.marks

@inline function getindex(p::VariablePoints, i::Integer)
    @boundscheck if !checkbounds(Bool, p.nakedpoints.points, i)
        throw(BoundsError(p, i))
    end
    MarkedPoint(@inbounds(p.nakedpoints.points[i]), @inbounds(p.marks[i]))
end

function show(io::IO, ::MIME"text/plain", pts::T) where {T<:VariablePoints}
    points_show_repl_preamble(io, pts)
    println(io, "  points: ", point_values(pts.nakedpoints))
    println(io, "   marks: ", pts.marks)
end

function show(io::IO, pts::T) where {T<:VariablePoints}
    if get(io, :typeinfo, T) != T
        print(io, T)
    end
    print(io, "(", bounds(pts), ',', collect(zip(point_values(pts)...)), ')')
end

nakedpoints(p::VariablePoints) = p.nakedpoints

function point_values(mp::VariablePoints, tb, te)
    pp = nakedpoints(mp)
    ib, ie = point_range(pp, tb, te)
    view(pp.points, ib:ie), view(mp.marks, ib:ie)
end

point_values(mp::VariablePoints) = nakedpoints(mp).points, mp.marks

translate(mp::VariablePoints, offset) =
    VariablePoints(translate(nakedpoints(mp), offset), mp.marks)

"""
    SubPoints{E,M,I,P} <: Points{E,1,I,M}

A lazy, zero-copy view of a [`Points`](@ref) collection restricted to a sub-interval.
Indexing uses a precomputed offset for O(1) element access.

# Constructors

    SubPoints(points::Points, interval::Interval)
    SubPoints(points::Points, (a, b))
    SubPoints(points::Points, a, b)

The sub-interval must be contained within `interval(points)` (throws `ArgumentError`
otherwise). Nesting `SubPoints` inside another `SubPoints` flattens automatically,
referencing the original underlying data.

See also [`maybe_subpoints`](@ref).
"""
struct SubPoints{E,M,I<:Interval{E},P<:Points{E,1,<:Any,M}} <: Points{E,1,I,M}
    points::P
    interval::I
    offset::Int
    function SubPoints{E,M,I,P}(
        points::P,
        bnd_int::I,
    ) where {E,M,I<:Interval{E},P<:Points{E,1,<:Any,M}}
        if ! validate_interval(bnd_int)
            throw(ArgumentError("Invalid interval"))
        end
        if ! is_subinterval(bnd_int, interval(points))
            throw(ArgumentError("sub interval is not contained in parent interval"))
        end
        offset = searchsortedfirst(points, bounds(bnd_int)[1]) - 1
        new(points, bnd_int, offset)
    end
end

function SubPoints(points::P, interval::I) where {E,M,I<:Interval{E},P<:Points{E,1,<:Any,M}}
    SubPoints{E,M,I,P}(points, interval)
end

function SubPoints(points::Points{E,1,<:Any,<:Any}, interval::NTuple{2,<:Any}) where {E}
    SubPoints(points, NakedInterval(interval))
end

function SubPoints(points::Points{E,1,<:Any,<:Any}, b, e) where {E}
    SubPoints(points, (convert(E, b), convert(E, e)))
end

function SubPoints(points::SubPoints, i::Interval)
    if !is_subinterval(i, interval(points))
        throw(ArgumentError("Sub interval is not contained in the parent interval"))
    end
    SubPoints(points.points, i)
end

function get_mark(spp::SubPoints{<:Any,<:Any,<:Any,<:MarkedPoints})
    np, nv = point_values(spp.points, bounds(spp)...)
    nv
end

size(spp::SubPoints) = (count(spp),)
@inline function getindex(spp::SubPoints, i::Integer)
    @boundscheck checkbounds(spp, i)
    @inbounds spp.points[i + spp.offset]
end

"""
    maybe_subpoints(points::Points, i::Interval) -> SubPoints or nothing

Return a [`SubPoints`](@ref) view if `i` overlaps with the domain of `points`, or
`nothing` if there is no intersection.
"""
function maybe_subpoints(points::Points, i::Interval)
    pi = interval(points)
    int_int = interval_intersect(pi, i)
    int_int === nothing ? nothing : SubPoints(points, int_int)
end

interval(spp::SubPoints) = spp.interval
count(spp::SubPoints) = count(spp.points, bounds(interval(spp))...)

function count(spp::SubPoints, b, e)
    int_int = interval_intersect(bounds(spp.interval)..., b, e)
    int_int === nothing ? zero(b) : count(spp.points, int_int...)
end

function point_values(spp::SubPoints{<:Any,<:NakedPoint}, b, e)
    int_int = interval_intersect(bounds(spp.interval)..., b, e)
    if int_int === nothing
        view(nakedpoints(spp.points).points, 1:0)
    else
        point_values(spp.points, int_int...)
    end
end

function point_values(spp::SubPoints{<:Any,<:MarkedPoint}, b, e)
    int_int = interval_intersect(bounds(spp.interval)..., b, e)
    if int_int === nothing
        np = nakedpoints(spp.points)
        view(np.points, 1:0), view(get_mark(spp.points), 1:0)
    else
        point_values(spp.points, int_int...)
    end
end

function nakedpoints(spp::SubPoints)
    vals = nakedvalues(spp.points, bounds(spp)...)
    NakedPoints(vals, spp.interval)
end

point_values(spp::SubPoints) = point_values(spp.points, bounds(spp.interval)...)

function show(io::IO, ::MIME"text/plain", pts::T) where {T<:SubPoints}
    points_show_repl_preamble(io, pts)
    print(io, "    ", pts)
end

function show(io::IO, pts::T) where {T<:SubPoints}
    if get(io, :typeinfo, T) != T
        print(io, T)
    end
    print(io, "(", bounds(pts), ',', collect(zip(point_values(pts)...)), ')')
end

subpoint_type(::Type{P}) where {E,M,P<:Points{E,1,<:Any,M}} =
    SubPoints{E,M,NakedInterval{E},P}
subpoint_type(::Type{P}) where {P<:SubPoints} = P

"""
    interval_intersections_subpoints(points::AbstractVector{<:Points}, intervals::AbstractVector{<:Interval}) -> Vector{SubPoints}

For each overlap between a points collection in `points` and an interval in `intervals`,
produce a [`SubPoints`](@ref) view. Both inputs must be sorted by start time.

This is the lower-level function behind [`points_intersects`](@ref).
"""
function interval_intersections_subpoints(
    points::AbstractVector{P},
    intervals::AbstractVector{<:Interval{E}},
) where {E,M,P<:Points{E,1,<:Any,M}}
    intervals_are_ordered(bounds, points) || error("points not well ordered")
    intervals_are_ordered(bounds, intervals) || error("intervals not well ordered")
    nb = length(intervals)
    outs = subpoint_type(P)[]
    ib = 1
    @inbounds for pts in points
        ab, ae = bounds(pts)
        while ib <= nb && bounds(intervals[ib])[2] <= ab
            ib += 1
        end
        ib > nb && break
        icheck = ib
        while icheck <= nb && bounds(intervals[icheck])[1] < ae
            push!(outs, maybe_subpoints(pts, intervals[icheck]))
            icheck += 1
        end
    end
    outs
end

"""
    points_intersects(pts1::AbstractVector{<:Points}, pts2::AbstractVector{<:Points}) -> (Vector{SubPoints}, Vector{SubPoints})

Restrict two vectors of [`Points`](@ref) to only the time ranges where both have
coverage. Returns a pair of `SubPoints` vectors aligned to the same intersection
intervals. Both inputs must be sorted by start time.

See also [`interval_intersections_subpoints`](@ref).
"""
function points_intersects(pts1::AbstractVector{<:Points}, pts2::AbstractVector{<:Points})
    ints1 = interval.(pts1)
    ints2 = interval.(pts2)
    newints = interval_intersections(ints1, ints2)
    newpts1 = interval_intersections_subpoints(pts1, newints)
    newpts2 = interval_intersections_subpoints(pts2, newints)
    return (newpts1, newpts2)
end

"""
    pp_downsamp(p::Points, b, e, resolution, [merge_func=pt_merge], [RetType=M]) -> VariablePoints

Downsample points within `[b, e]` by merging consecutive points closer than `resolution`.
Adjacent points whose gap is less than `resolution` are merged using `merge_func`. Each
merged point receives a count mark (via [`push_mark`](@ref)) recording how many original
points were combined.

The default `merge_func` is [`pt_merge`](@ref) (mean of timestamps and marks).
`RetType` specifies the element type returned by `merge_func` for type stability;
it defaults to `M`, the point element type of the input collection.

See also [`pt_merge`](@ref), [`pt_extent_merge`](@ref), [`pop_marks`](@ref).
"""
function pp_downsamp(
    p::Points{E,1,<:Any,M},
    b,
    e,
    resolution,
    merge_func::Function = pt_merge,
    ::Type{RetType} = M, # merge_func return type
) where {E,M,RetType}
    pnts = points(p, b, e)
    np = length(pnts)
    OutType = push_mark_type(RetType, Int)
    downsamped_points = OutType[]
    @inbounds if np > 0
        range_st = 1
        for i = 2:np
            if pnts[i].point - pnts[i-1].point >= resolution
                mergecount = n_ndx(range_st, i - 1)
                mergedpnt = merge_func(view(pnts, range_st:(i-1)))
                push!(downsamped_points, push_mark(mergedpnt, mergecount))
                range_st = i
            end
        end
        mergecount = n_ndx(range_st, np)
        mergedpnt = merge_func(view(pnts, range_st:np))
        push!(downsamped_points, push_mark(mergedpnt, mergecount))
    end
    VariablePoints(downsamped_points, b, e)
end

"""
    pt_merge(pts::AbstractVector{<:NakedPoint}) -> NakedPoint
    pt_merge(pts::AbstractVector{<:MarkedPoint}) -> MarkedPoint

Merge a group of points by averaging their timestamps (and marks, for
[`MarkedPoint`](@ref)). Used as the default merge function in [`pp_downsamp`](@ref).
"""
function pt_merge(pts::AbstractVector{<:NakedPoint{<:Number}})
    NakedPoint(mean(point_values(pts)))
end
function pt_merge(pts::AbstractVector{<:MarkedPoint{<:Number,<:Number}})
    pt_vals, mark_vals = point_values(pts)
    MarkedPoint(mean(pt_vals), mean(mark_vals))
end

"""
    pt_extent_merge(pts::AbstractVector{<:MarkedPoint}) -> MarkedPoint
    pt_extent_merge(pts::AbstractVector{<:NakedPoint}) -> MarkedPoint

Merge a group of points by averaging timestamps and recording the extent `(min, max)`.
For [`MarkedPoint`](@ref) inputs, returns `MarkedPoint(mean_time, ((min_time, max_time), mean_mark))`.
For [`NakedPoint`](@ref) inputs, returns `MarkedPoint(mean_time, (min_time, max_time))`.

Alternative merge function for [`pp_downsamp`](@ref).
"""
function pt_extent_merge(pts::AbstractVector{<:MarkedPoint{<:Number,<:Number}})
    pt_vals, mark_vals = point_values(pts)
    pt_mean = mean(pt_vals)
    pt_extent = (minimum(pt_vals), maximum(pt_vals))
    mark_mean = mean(mark_vals)
    MarkedPoint(pt_mean, (pt_extent, mark_mean))
end

function pt_extent_merge(pts::AbstractVector{<:NakedPoint})
    pt_vals = point_values(pts)
    pt_mean = mean(pt_vals)
    pt_extent = (minimum(pt_vals), maximum(pt_vals))
    MarkedPoint(pt_mean, pt_extent)
end

"""
    pop_marks(p::VariablePoints) -> VariablePoints

Strip the outermost element from tuple marks on all points. Each mark `(a, b)` becomes
`b`. This is the collection-level counterpart of [`pop_mark`](@ref) and is useful for
removing the merge-count mark added by [`pp_downsamp`](@ref).
"""
function pop_marks(p::VariablePoints)
    pt_vals, mark_vals = point_values(p)
    popped_marks = map(m -> m[2], mark_vals)
    VariablePoints(pt_vals, popped_marks, interval(p))
end

"""
    join_points(pt::Points) -> NakedPoints or VariablePoints
    join_points(pt1::Points, pt2::Points, pts::Points...) -> NakedPoints or VariablePoints

Materialise or merge point collections. The single-argument form copies a (possibly
lazy) `Points` into a concrete `NakedPoints` or `VariablePoints`. The multi-argument
form concatenates points and sorts the result; the output interval is the union of all
input intervals. All arguments must have the same point type (all naked or all marked).
"""
function join_points(pt::Points{<:Any,<:Any,<:Any,P}) where {P<:NakedPoint}
    newint = nakedinterval(pt)
    NakedPoints(collect(point_values(pt)), newint)
end

function join_points(pt::Points{<:Any,<:Any,<:Any,P}) where {P<:MarkedPoint}
    newint = nakedinterval(pt)
    VariablePoints(collect.(point_values(pt))..., newint)
end

function join_points(
    pt1::Points{<:Any,<:Any,<:Any,P},
    pt2::Points{<:Any,<:Any,<:Any,P},
) where {P<:NakedPoint}
    newbnds = overlap_interval_union(bounds(pt1), bounds(pt2))
    newint = NakedInterval(newbnds)
    newvals = sort!(vcat(point_values(pt1), point_values(pt2)))
    NakedPoints(newvals, newint)
end

function join_points(
    pt1::Points{<:Any,<:Any,<:Any,P},
    pt2::Points{<:Any,<:Any,<:Any,P},
) where {P<:MarkedPoint}
    newbnds = overlap_interval_union(bounds(pt1), bounds(pt2))
    newint = NakedInterval(newbnds)
    times1, marks1 = point_values(pt1)
    times2, marks2 = point_values(pt2)
    newtimes = vcat(times1, times2)
    sp = sortperm(newtimes)
    newtimes = newtimes[sp]
    newmarks = vcat(marks1, marks2)[sp]
    VariablePoints(newtimes, newmarks, newint)
end

function join_points(
    pt1::Points{<:Any,<:Any,<:Any,P},
    pt2::Points{<:Any,<:Any,<:Any,P},
    pts::Vararg{Points{<:Any,<:Any,<:Any,P}},
) where {P<:Point}
    newpts = join_points(pt1, pt2)
    join_points(newpts, pts...)
end
