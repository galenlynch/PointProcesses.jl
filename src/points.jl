# If not NakedPoints, expected to have a field "nakedpoints" that is a
# NakedPoints or implement a nakedpoints method that will yield a
# simple naked points
# Alternatively, implement duration, point_range, and count
# point_range(pts, b, e) -> (ib, ie) gives the indices of points between b, e
# count is the number of points
# Should also support interval function that gives underlying interval
abstract type Points{E, N, I<:Interval{E}, T<:Point{E}} <: AbstractVector{T} end
const MarkedPoints{E, N, I, M} = Points{E, N, I, MarkedPoint{E, M}}

IndexStyle(::Points) = IndexLinear()
setindex!(::Points) = throw(ReadOnlyMemoryError())

rate(p::Points) = count(p) / duration(p)
rate(p::Points, b, e) = count(p, b, e) / (e - b)
function rate(ps::AbstractVector{<:Points})
    if isempty(ps)
        nothing
    else
        sum(count, ps) / sum(duration, ps)
    end
end

interval(p::Points) = interval(nakedpoints(p))
measure(p::Points) = measure(interval(p))
duration(p::Points) = measure(p)
time_interval(p::Points) = interval(p)
bounds(p::Points) = bounds(interval(p))
function points(p::Points{<:Any, <:Any, <:Any, M}, args...) where M
    M.(point_values(p, args...)...)
end
nakedvalues(p::Points, b, e) = point_values(nakedpoints(p), b, e)
nakedvalues(p::Points) = nakedvalues(p, bounds(p)...)

# I do not check, nor require, that points are strictly increasing
struct NakedPoints{
    E<:Number, I<:Interval{E}, A<:AbstractVector{E}
} <: Points{E, 1, I, NakedPoint{E}}
    points::A
    interval::I
    function NakedPoints{E,I,A}(
        points::A, interval::I
    ) where {E, I<:Interval{E}, A<:AbstractVector{E}}
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
    points::A, interval::I, check::Bool = true
) where {E, I<:Interval{E}, A<:AbstractVector{E}}
    if check && ! issorted(points)
        sort!(points)
    end
    NakedPoints{E, I, A}(points, interval)
end

function NakedPoints(
    points::AbstractVector{E}, interval::NTuple{2, <:Any}, check::Bool = true
) where E
    NakedPoints(points, NakedInterval(convert(Tuple{E, E}, interval)), check)
end

function NakedPoints(points::AbstractVector{E}, a::Number, b::Number) where E
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

validate_interval(int::NTuple{2, Number}) = int[2] >= int[1]
validate_interval(i::Interval) = validate_interval(bounds(i))

function points_show_repl_preamble(io::IO, pts::T) where T<:Points
    println(
        io, length(pts), "–point ", T, " defined on interval ", bounds(pts), ":"
    )
end
function show(io::IO, ::MIME"text/plain", pts::T) where T<:NakedPoints
    points_show_repl_preamble(io, pts)
    print(io, "    ", point_values(pts))
end

count(p::NakedPoints) = length(p.points)
function count(p::NakedPoints, range_start, range_end)
    ib, ie = point_range(p, range_start, range_end)
    max(zero(ib), n_ndx(ib, ie))
end

function point_range(p::NakedPoints{E, <:Any, <:Any}, b::E, e::E) where E
    b <= e || throw(ArgumentError("beginning and end not well ordered"))
    ib = searchsortedfirst(p.points, b)
    ie = searchsortedlast(p.points, e)
    ib, ie
end
function point_range(p::NakedPoints{E, <:Any, <:Any}, b, e) where E
    point_range(p, convert(E, b), convert(E, e))
end


function point_values(p::NakedPoints, b, e)
    ib, ie = point_range(p, b, e)
    view(p.points, ib:ie)
end
point_values(p::NakedPoints) = p.points

points(p::NakedPoints, args...) = NakedPoint.(point_values(p, args...))

nakedpoints(p::NakedPoints) = p

function translate(p::NakedPoints, offset)
    rb, re = bounds(p)
    NakedPoints(p.points .+ offset, (rb + offset, re + offset))
end

point_range(p::Points, args...) = point_range(nakedpoints(p), args...)
count(p::Points, args...) = count(nakedpoints(p), args...)

struct VariablePoints{
    E, I<:Interval{E}, P<:NakedPoints{E, I, <:Any}, M, A<:AbstractVector{M}
} <: Points{E, 1, I, MarkedPoint{E, M}}
    nakedpoints::P
    marks::A
    function VariablePoints{E, I, P, M, A}(
        nakedpoints::P, marks::A
    ) where {E, I<:Interval{E}, P<:NakedPoints{E}, M, A<:AbstractVector{M}}
        if count(nakedpoints) != length(marks)
            throw(DimensionMismatch("point process must be the same size as marks"))
        end
        new(nakedpoints, marks)
    end
end

function VariablePoints(
    nakedpoints::P, marks::A
) where {
    E,
    I<:Interval{E},
    P<:NakedPoints{E, I, <:Any},
    M,
    A<:AbstractVector{M}
}
    VariablePoints{E, I, P, M, A}(nakedpoints, marks)
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
    @compat ps = Vector{E}(undef, np)
    @compat mks = Vector{M}(undef, np)
    @inbounds @simd for i in 1:np
        ps[i] = pts[i].point
        mks[i] = pts[i].mark
    end
    VariablePoints(ps, mks, args...)
end
size(p::VariablePoints) = size(nakedpoints(p))

@inline function getindex(p::VariablePoints, i::Integer)
    @boundscheck if !checkbounds(Bool, p.nakedpoints.points, i)
        throw(BoundsError(p, i))
    end
    MarkedPoint(@inbounds(p.nakedpoints.points[i]), @inbounds(p.marks[i]))
end

function show(io::IO, ::MIME"text/plain", pts::T) where T<:VariablePoints
    points_show_repl_preamble(io, pts)
    println(io, "  points: ", point_values(pts.nakedpoints))
    println(io, "   marks: ", pts.marks)
end

function show(io::IO, pts::T) where T<:VariablePoints
    if get(io, :typeinfo, T) != T
        print(io, T)
    end
    print(
        io, "(", bounds(pts), ',', collect(zip(point_values(pts)...)), ')'
    )
end

nakedpoints(p::VariablePoints) = p.nakedpoints

function point_values(mp::VariablePoints, tb, te)
    pp = nakedpoints(mp)
    ib, ie = point_range(pp, tb, te)
    view(pp.points, ib:ie), view(mp.marks, ib:ie)
end

point_values(mp::VariablePoints) = nakedpoints(mp).points, mp.marks

translate(mp::VariablePoints, offset) = VariablePoints(
    translate(nakedpoints(mp), offset), mp.marks
)

struct SubPoints{E, M, I<:Interval{E}, P<:Points{E, 1, <:Any, M}} <: Points{E, 1, I, M}
    points::P
    interval::I
    function SubPoints{E, M, I, P}(
        points::P, bnd_int::I
    ) where {E, M, I<:Interval{E}, P<:Points{E, 1, <:Any, M}}
        if ! validate_interval(bnd_int)
            throw(ArgumentError("Invalid interval"))
        end
        if ! is_subinterval(bnd_int, interval(points))
            throw(ArgumentError("sub interval is not contained in parent interval"))
        end
        new(points, bnd_int)
    end
end

function SubPoints(
    points::P, interval::I
) where {E, M, I<:Interval{E}, P<:Points{E, 1, <:Any, M}}
    SubPoints{E, M, I, P}(points, interval)
end

function SubPoints(
    points::Points{E, 1, <:Any, <:Any}, interval::NTuple{2, <:Any}
) where E
    SubPoints(points, NakedInterval(interval))
end

function SubPoints(points::Points{E, 1, <:Any, <:Any}, b, e) where E
    SubPoints(points, (convert(E, b), convert(E, e)))
end

function SubPoints(points::SubPoints, i::Interval)
    if !is_subinterval(i, interval(points))
        throw(ArgumentError("Sub interval is not contained in the parent interval"))
    end
    SubPoints(points.points, i)
end

size(spp::SubPoints) = (count(spp),)
@inline function getindex(spp::SubPoints, i::Integer)
    ib = searchsortedfirst(spp.points, bounds(spp)[1])
    @boundscheck ib > length(spp.points) && throw(BoundsError(spp, i))
    @boundscheck if !checkbounds(Bool, nakedvalues(spp.points), i)
        throw(BoundsError(spp, i))
    end
    @inbounds spp.points[i + ib - 1]
end

function maybe_subpoints(points::Points, i::Interval)
    pi = interval(points)
    int_int = interval_intersect(pi, i)
    int_int == nothing ? nothing : SubPoints(points, int_int)
end

interval(spp::SubPoints) = spp.interval
count(spp::SubPoints) = count(spp.points, bounds(interval(spp))...)

function count(spp::SubPoints, b, e)
    int_int = interval_intersect(bounds(interval(spp.interval))..., b, e)
    int_int == nothing ? zero(b) : count(spp.points, int_int...)
end

function point_values(spp::SubPoints, b, e)
    int_int = interval_intersect(bounds(interval(spp.interval))..., b, e)
    if int_int == nothing
        res = points_values(spp.points, 1, 0)
    else
        res = point_values(spp.points, int_int...)
    end
    res
end

function nakedpoints(spp::SubPoints)
    vals = nakedvalues(spp.points, bounds(spp)...)
    NakedPoints(vals, spp.interval)
end

point_values(spp::SubPoints) = point_values(spp.points, bounds(spp.interval)...)

function show(io::IO, ::MIME"text/plain", pts::T) where T<:SubPoints
    points_show_repl_preamble(io, pts)
    print(io, "    ", pts)
end

function show(pts::SubPoints)
    if get(io, :typeinfo, T) != T
        print(io, T)
    end
    print(
        io, "(", bounds(pts), ',', collect(zip(point_values(pts)...)), ')'
    )
end

function pp_downsamp(
    p::Points{E, 1, <:Any, M},
    b,
    e,
    resolution,
    merge_func::Function = pt_merge,
    ::Type{RetType} = M # merge_func return type
) where {E, M, RetType}
    pnts = points(p, b, e)
    np = length(pnts)
    @compat downsamped_points = Vector{push_mark_type(RetType, Int)}(undef, np)
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

function pop_marks(p::VariablePoints)
    pt_vals, mark_vals = point_values(p)
    popped_marks = map(m -> m[2], mark_vals)
    VariablePoints(pt_vals, popped_marks, interval(p))
end
