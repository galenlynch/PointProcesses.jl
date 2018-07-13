# If not NakedPointProcess, expected to have a field "pointprocess" that is a
# NakedPointProcess or implement a pointprocess method that will yield a
# simplepointprocess
# Alternatively, implement duration, point_range, and count
abstract type PointProcess{E, T<:Point{E}} end
abstract type MarkedPointProcess{E, M} <: PointProcess{E, MarkedPoint{E, M}} end

rate(p::PointProcess) = count(p) / duration(p)
rate(p::PointProcess, b, e) = count(p, b, e) / (e - b)

# I do not check, nor require, that points are strictly increasing
struct NakedPointProcess{
    E<:Number, A<:AbstractVector{E}
} <: PointProcess{E, NakedPoint{E}}
    interval::NTuple{2, E}
    points::A
    function NakedPointProcess{E,A}(
        points::A, interval::NTuple{2, E}
    ) where {E, A<:AbstractVector{E}}
        issorted(points) || throw(ArgumentError("Times is not sorted"))
        validate_interval(interval) || throw(ArgumentError("Interval not valid"))
        if ! isempty(points)
            if points[1] < interval[1] || points[end] > interval[2]
                throw(ArgumentError("Points exceed interval"))
            end
        end
        new(interval, points)
    end
end

function NakedPointProcess(
    points::A, interval::NTuple{2, E}, check::Bool = true
) where {E, A<:AbstractVector{E}}
    if check && ! issorted(points)
        sort!(points)
    end
    NakedPointProcess{E, A}(points, interval)
end

function NakedPointProcess(
    points::AbstractVector{E}, interval::NTuple{2, <:Any}
) where {E}
    NakedPointProcess(points, (convert.(E, interval)...))
end

function NakedPointProcess(points::AbstractVector{E}, a::Number, b::Number) where E
    NakedPointProcess(points, (convert(E, a), convert(E, b)))
end

function NakedPointProcess(points::AbstractVector)
    isempty(points) && throw(ArgumnetError("points cannot be empty"))
    mypoints = copy(points)
    sort!(mypoints)
    NakedPointProcess(mypoints, (mypoints[1], mypoints[end]), false)
end

function NakedPointProcess(points::AbstractVector{<:NakedPoint}, args...)
    NakedPointProcess(getfield.(points, :point), args...)
end

validate_interval(int::NTuple{2, Number}) = int[2] >= int[1]

count(p::NakedPointProcess) = length(p.points)
function count(p::NakedPointProcess, range_start, range_end)
    ib, ie = point_range(p, range_start, range_end)
    max(zero(ib), n_ndx(ib, ie))
end

function point_range(p::NakedPointProcess{E, <:Any}, b::E, e::E) where E
    b <= e || throw(ArgumentError("beginning and end not well ordered"))
    ib = searchsortedfirst(p.points, b)
    ie = searchsortedlast(p.points, e)
    ib, ie
end
function point_range(p::NakedPointProcess{E, <:Any}, b, e) where E
    point_range(p, convert(E, b), convert(E, e))
end

duration(p::NakedPointProcess) = p.interval[2] - p.interval[1]
time_interval(p::NakedPointProcess) = p.interval

function points(p::NakedPointProcess, b, e)
    ib, ie = point_range(p, b, e)
    NakedPoint.(view(p.points, ib:ie))
end
points(p::NakedPointProcess) = NakedPoint.(p.points)

point_values(p::PointProcess, args...) = point_values(points(p, args...))

pointprocess(p::NakedPointProcess) = p
pointprocess(p::PointProcess) = p.pointprocess

duration(p::PointProcess) = duration(pointprocess(p))
time_interval(p::PointProcess) = time_interval(pointprocess(p))
point_range(p::PointProcess, args...) = point_range(pointprocess(p), args...)
count(p::PointProcess, args...) = count(pointprocess(p), args...)


struct VariablePointProcess{
    E, P<:NakedPointProcess{E}, M, A<:AbstractVector{M}
} <: MarkedPointProcess{E, M}
    pointprocess::P
    marks::A
    function VariablePointProcess{E, P, M, A}(
        pointprocess::P, marks::A
    ) where {E, P<:NakedPointProcess{E}, M, A<:AbstractVector{M}}
        if count(pointprocess) != length(marks)
            throw(DimensionMismatch("point process must be the same size as marks"))
        end
        new(pointprocess, marks)
    end
end

function VariablePointProcess(
    pointprocess::P, marks::A
) where {E, P<:NakedPointProcess{E}, M, A<:AbstractVector{M}}
    VariablePointProcess{E, P, M, A}(pointprocess, marks)
end

function VariablePointProcess(
    points::AbstractVector, marks::AbstractVector, args...
)
    mypoints = copy(points)
    mymarks = copy(marks)
    sp = sortperm(mypoints)
    mypoints = mypoints[sp]
    mymarks = mymarks[sp]
    pp = NakedPointProcess(points, args...)
    VariablePointProcess(pp, marks)
end

function VariablePointProcess(points::AbstractVector{<:MarkedPoint}, args...)
    VariablePointProcess(
        getfield.(points, :point), getfield.(points, :mark), args...
    )
end

function points(mp::MarkedPointProcess{E, M}, ib, ie) where {E, M}
    pp = pointprocess(mp)
    ib, ie = point_range(pp, ib, ie)
    idx_range = ib:ie
    out = Vector{MarkedPoint{E, M}}(length(idx_range))
    for (out_idx, data_idx) in enumerate(idx_range)
        out[out_idx] = MarkedPoint(pp.points[data_idx], mp.marks[data_idx])
    end
    out
end
function points(mp::MarkedPointProcess)
    MarkedPoint.(pointprocess(mp).points, mp.marks)
end


struct SubPointProcess{E, M, P<:PointProcess{E, M}} <: PointProcess{E, M}
    pointprocess::P
    interval::NTuple{2, E}
    function SubPointProcess{E, M, P}(
        pointprocess::P, interval::NTuple{2, E}
    ) where {E, M, P<:PointProcess{E, M}}
        if ! validate_interval(interval)
            throw(ArgumentError("Invalid interval"))
        end
        if interval[1] < pointprocess.interval[1] ||
            interval[2] > pointprocess.interval[2]
            throw(ArgumentError("sub interval is not contained in parent interval"))
        end
        new(pointprocess, interval)
    end
end

function SubPointProcess(
    pointprocess::P, interval::NTuple{2, E}
) where {E, M, P<:PointProcess{E, M}}
    SubPointProcess{E, M, P}(pointprocess, interval)
end

function SubPointProcess(
    pointprocess::PointProcess{E, <:Any}, interval::NTuple{2, <:Any}
) where E
    SubPointProcess(pointprocess, (convert.(E, interval)...))
end

function SubPointProcess(pointprocess::PointProcess{E, <:Any}, b, e) where E
    SubPointProcess(pointprocess, (convert(E, b), convert(E, e)))
end

duration(spp::SubPointProcess) = spp.interval[2] - spp.interval[1]
time_interval(spp::SubPointProcess) = spp.interval
count(spp::SubPointProcess) = count(spp.pointprocess, spp.interval...)
function count(spp::SubPointProcess, b, e)
    int_int = interval_intersect(spp.interval..., b, e)
    isempty(int_int) ? zero(b) : count(spp.pointprocess, int_int...)
end

function points(spp::SubPointProcess, b, e)
    int_int = interval_intersect(spp.interval..., b, e)
    if isempty(int_int)
        res = points(spp.pointprocess, 1, 0)
    else
        res = points(spp.pointprocess, int_int...)
    end
    res
end

function pp_downsamp(
    p::PointProcess{E, M},
    b,
    e,
    resolution,
    merge_func::Function # Must return type M
) where {E, M}
    pnts = points(p, b, e)
    np = length(pnts)
    downsamped_points = Vector{prepend_mark_type(M, Int)}(np)
    if np > 0
        range_st = 1
        out_idx = 0
        for i in 2:np
            if pnts[i].point - pnts[i - 1].point >= resolution # End last range
                out_idx += 1
                mergecount = n_ndx(range_st, i - 1)
                mergedpnt = merge_func(view(pnts, range_st:(i - 1)))
                downsamped_points[out_idx] = prepend_mark(mergedpnt, mergecount)
                range_st = i
            end
        end
        # Handle the last point
        out_idx += 1
        mergecount = n_ndx(range_st, np)
        mergedpnt = merge_func(view(pnts, range_st:np))
        downsamped_points[out_idx] = prepend_mark(mergedpnt, mergecount)
    end
    resize!(downsamped_points, out_idx)
    VariablePointProcess(downsamped_points, b, e)
end
function pp_downsamp(
    p::NakedPointProcess{<:AbstractFloat, <:Any}, b, e, resolution
)
    pp_downsamp(p, b, e, resolution, naked_merge)
end
function naked_merge(pts::AbstractVector{<:NakedPoint{<:AbstractFloat}})
    NakedPoint(mean(point_values(pts)))
end
