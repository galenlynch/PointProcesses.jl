# If not NakedPoints, expected to have a field "nakedpoints" that is a
# NakedPoints or implement a nakedpoints method that will yield a
# simplenakedpoints
# Alternatively, implement duration, point_range, and count
abstract type Points{E, N, T<:Point{E}} end
MarkedPoints{E, N, M} = Points{E, N, MarkedPoint{E, M}}

rate(p::Points) = count(p) / duration(p)
rate(p::Points, b, e) = count(p, b, e) / (e - b)

points(p::Points{<:Any, <:Any, M}, args...) where M = M.(point_values(p, args...)...)

# I do not check, nor require, that points are strictly increasing
struct NakedPoints{
    E<:Number, A<:AbstractVector{E}
} <: Points{E, 1, NakedPoint{E}}
    interval::NTuple{2, E}
    points::A
    function NakedPoints{E,A}(
        points::A, interval::NTuple{2, E}
    ) where {E, A<:AbstractVector{E}}
        issorted(points) || throw(ArgumentError("Times is not sorted"))
        validate_interval(interval) || throw(ArgumentError("Interval not valid"))
        if ! isempty(points)
            @inbounds if points[1] < interval[1] || points[end] > interval[2]
                throw(ArgumentError("Points exceed interval"))
            end
        end
        new(interval, points)
    end
end

function NakedPoints(
    points::A, interval::NTuple{2, E}, check::Bool = true
) where {E, A<:AbstractVector{E}}
    if check && ! issorted(points)
        sort!(points)
    end
    NakedPoints{E, A}(points, interval)
end

function NakedPoints(
    points::AbstractVector{E}, interval::NTuple{2, <:Any}
) where {E}
    NakedPoints(points, (convert.(E, interval)...))
end

function NakedPoints(points::AbstractVector{E}, a::Number, b::Number) where E
    NakedPoints(points, (convert(E, a), convert(E, b)))
end

function NakedPoints(points::AbstractVector)
    isempty(points) && throw(ArgumnetError("points cannot be empty"))
    mypoints = copy(points)
    sort!(mypoints)
    NakedPoints(mypoints, (mypoints[1], mypoints[end]), false)
end

function NakedPoints(points::AbstractVector{<:NakedPoint}, args...)
    NakedPoints(getfield.(points, :point), args...)
end

validate_interval(int::NTuple{2, Number}) = int[2] >= int[1]

count(p::NakedPoints) = length(p.points)
function count(p::NakedPoints, range_start, range_end)
    ib, ie = point_range(p, range_start, range_end)
    max(zero(ib), n_ndx(ib, ie))
end

function point_range(p::NakedPoints{E, <:Any}, b::E, e::E) where E
    b <= e || throw(ArgumentError("beginning and end not well ordered"))
    ib = searchsortedfirst(p.points, b)
    ie = searchsortedlast(p.points, e)
    ib, ie
end
function point_range(p::NakedPoints{E, <:Any}, b, e) where E
    point_range(p, convert(E, b), convert(E, e))
end

duration(p::NakedPoints) = p.interval[2] - p.interval[1]
time_interval(p::NakedPoints) = p.interval

function point_values(p::NakedPoints, b, e)
    ib, ie = point_range(p, b, e)
    view(p.points, ib:ie)
end
point_values(p::NakedPoints) = p.points

points(p::NakedPoints, args...) = NakedPoint.(point_values(p, args...))

nakedpoints(p::NakedPoints) = p
nakedpoints(p::Points) = p.nakedpoints

duration(p::Points) = duration(nakedpoints(p))
time_interval(p::Points) = time_interval(nakedpoints(p))
point_range(p::Points, args...) = point_range(nakedpoints(p), args...)
count(p::Points, args...) = count(nakedpoints(p), args...)

struct VariablePoints{
    E, P<:NakedPoints{E, <:Any}, M, A<:AbstractVector{M}
} <: Points{E, 1, MarkedPoint{E, M}}
    nakedpoints::P
    marks::A
    function VariablePoints{E, P, M, A}(
        nakedpoints::P, marks::A
    ) where {E, P<:NakedPoints{E}, M, A<:AbstractVector{M}}
        if count(nakedpoints) != length(marks)
            throw(DimensionMismatch("point process must be the same size as marks"))
        end
        new(nakedpoints, marks)
    end
end

function VariablePoints(
    nakedpoints::P, marks::A
) where {E, P<:NakedPoints{E}, M, A<:AbstractVector{M}}
    VariablePoints{E, P, M, A}(nakedpoints, marks)
end

function VariablePoints(
    points::AbstractVector, marks::AbstractVector, args...
)
    mypoints = copy(points)
    mymarks = copy(marks)
    sp = sortperm(mypoints)
    mypoints = mypoints[sp]
    mymarks = mymarks[sp]
    pp = NakedPoints(points, args...)
    VariablePoints(pp, marks)
end

function VariablePoints(
    pts::AbstractVector{<:MarkedPoint{E, M}}, args...
) where {E, M}
    np = length(pts)
    ps = Vector{E}(np)
    mks = Vector{M}(np)
    @inbounds @simd for i in 1:np
        ps[i] = pts[i].point
        mks[i] = pts[i].mark
    end
    VariablePoints(ps, mks, args...)
end

function point_values(mp::MarkedPoints, ib, ie)
    pp = nakedpoints(mp)
    ib, ie = point_range(pp, ib, ie)
    view(pp.points, ib:ie), view(mp.marks, ib:ie)
end

point_values(mp::MarkedPoints) = nakedpoints(mp).points, mp.marks


struct SubPoints{E, M, P<:Points{E, 1, M}} <: Points{E, 1, M}
    points::P
    interval::NTuple{2, E}
    function SubPoints{E, M, P}(
        points::P, interval::NTuple{2, E}
    ) where {E, M, P<:Points{E, 1, M}}
        if ! validate_interval(interval)
            throw(ArgumentError("Invalid interval"))
        end
        parent_interval = time_interval(points)
        @inbounds if interval[1] < parent_interval[1] ||
            interval[2] > parent_interval[2]
            throw(ArgumentError("sub interval is not contained in parent interval"))
        end
        new(points, interval)
    end
end

function SubPoints(
    points::P, interval::NTuple{2, E}
) where {E, M, P<:Points{E, 1, M}}
    SubPoints{E, M, P}(points, interval)
end

function SubPoints(
    points::Points{E, 1, <:Any}, interval::NTuple{2, <:Any}
) where E
    SubPoints(points, (convert.(E, interval)...))
end

function SubPoints(points::Points{E, 1, <:Any}, b, e) where E
    SubPoints(points, (convert(E, b), convert(E, e)))
end

duration(spp::SubPoints) = spp.interval[2] - spp.interval[1]
time_interval(spp::SubPoints) = spp.interval
count(spp::SubPoints) = count(spp.points, spp.interval...)
function count(spp::SubPoints, b, e)
    int_int = interval_intersect(spp.interval..., b, e)
    isempty(int_int) ? zero(b) : count(spp.points, int_int...)
end

function point_values(spp::SubPoints, b, e)
    int_int = interval_intersect(spp.interval..., b, e)
    if isempty(int_int)
        res = points_values(spp.points, 1, 0)
    else
        res = point_values(spp.points, int_int...)
    end
    res
end

function pp_downsamp(
    p::Points{E, 1, M},
    b,
    e,
    resolution,
    merge_func::Function = pt_merge,
    ::Type{RetType} = M # merge_func return type
) where {E, M, RetType}
    pnts = points(p, b, e)
    np = length(pnts)
    downsamped_points = Vector{push_mark_type(RetType, Int)}(np)
    out_idx = 0
    @inbounds if np > 0
        range_st = 1
        for i in 2:np
            if pnts[i].point - pnts[i - 1].point >= resolution # End last range
                out_idx += 1
                mergecount = n_ndx(range_st, i - 1)
                mergedpnt = merge_func(view(pnts, range_st:(i - 1)))
                downsamped_points[out_idx] = push_mark(mergedpnt, mergecount)
                range_st = i
            end
        end
        # Handle the last point
        out_idx += 1
        mergecount = n_ndx(range_st, np)
        mergedpnt = merge_func(view(pnts, range_st:np))
        downsamped_points[out_idx] = push_mark(mergedpnt, mergecount)
    end
    resize!(downsamped_points, out_idx)
    VariablePoints(downsamped_points, b, e)
end

function pt_merge(pts::AbstractVector{<:NakedPoint{<:Number}})
    NakedPoint(mean(point_values(pts)))
end
function pt_merge(
    pts::AbstractVector{<:MarkedPoint{<:Number, <:Number}}
)
    pt_vals, mark_vals = point_values(pts)
    MarkedPoint(mean(pt_vals), mean(mark_vals))
end

function pt_extent_merge(pts::AbstractVector{<:MarkedPoint{<:Number, <:Number}})
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

function pop_marks(p::VariablePoints{<:Any, <:Any, <:Tuple{<:Any, Vararg}, <:Any})
    pt_vals, mark_vals = point_values(p)
    popped_marks = map(m -> m[2], mark_vals)
    VariablePoints(pt_vals, popped_marks, time_interval(p))
end
