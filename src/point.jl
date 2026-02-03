"""
    Point{E}

Abstract supertype for timestamped event types, parameterised by the element type `E`
of the timestamp. Concrete subtypes are [`NakedPoint`](@ref) and [`MarkedPoint`](@ref).

`Point` values support `isless` comparison with `Number` values, delegating to
[`nakedpointvalue`](@ref).
"""
abstract type Point{E} end # must have point field

isless(pt::Point, n::Number) = isless(nakedpointvalue(pt), n)
isless(n::Number, pt::Point) = isless(n, nakedpointvalue(pt))

"""
    NakedPoint{E} <: Point{E}

A bare timestamp with no metadata. Wraps a single value of type `E`.
"""
struct NakedPoint{E} <: Point{E}
    point::E
end

function show(io::IO, pt::T) where {T<:NakedPoint}
    get(io, :typeinfo, nothing) != T && print(io, T)
    print(io, pt.point)
end

"""
    nakedpointvalue(pt::Point) -> E

Return the raw timestamp value from a [`Point`](@ref).
"""
nakedpointvalue(pt::NakedPoint) = pt.point

"""
    point_values(pts)

Extract the underlying value arrays from a collection of points.

For a vector of [`NakedPoint`](@ref), returns a vector of timestamps.
For a vector of [`MarkedPoint`](@ref), returns `(timestamps, marks)`.
For [`Points`](@ref) subtypes, see [`NakedPoints`](@ref), [`VariablePoints`](@ref),
and [`SubPoints`](@ref) for range-query forms `point_values(pts, b, e)`.
"""
point_values(pts::AbstractVector{<:NakedPoint}) = getfield.(pts, :point)

"""
    MarkedPoint{E,M} <: Point{E}

A timestamp of type `E` paired with a mark of type `M`. Marks can be any type —
numbers, symbols, tuples, or custom structs.

See also [`push_mark`](@ref), [`pop_mark`](@ref).
"""
struct MarkedPoint{E,M} <: Point{E}
    point::E
    mark::M
end

function show(io::IO, pt::T) where {T<:MarkedPoint}
    get(io, :typeinfo, nothing) != T && print(io, T)
    print(io, '(', pt.point, ", ", pt.mark, ')')
end

nakedpointvalue(pt::MarkedPoint) = pt.point

function point_values(pts::AbstractVector{MarkedPoint{E,M}}) where {E,M}
    np = length(pts)
    pnt_vals = Vector{E}(undef, np)
    marks = Vector{M}(undef, np)
    @inbounds for (i, p) in enumerate(pts)
        pnt_vals[i] = p.point
        marks[i] = p.mark
    end
    pnt_vals, marks
end

"""
    push_mark(p::NakedPoint, mark) -> MarkedPoint
    push_mark(mp::MarkedPoint, newmark) -> MarkedPoint

Add a mark to a point. On a [`NakedPoint`](@ref), creates a [`MarkedPoint`](@ref).
On a [`MarkedPoint`](@ref), nests the new mark into a tuple `(newmark, existing_mark)`,
forming a mark stack.

See also [`pop_mark`](@ref).
"""
push_mark(mp::MarkedPoint, newmark) = MarkedPoint(mp.point, (newmark, mp.mark))
push_mark(p::NakedPoint, mark) = MarkedPoint(p.point, mark)

"""
    pop_mark(mp::MarkedPoint{<:Any,<:Tuple}) -> (MarkedPoint, mark)

Remove the outermost mark from a tuple-marked [`MarkedPoint`](@ref), returning the
modified point and the removed mark. If the mark tuple is empty, returns `(mp, nothing)`.

See also [`push_mark`](@ref), [`pop_marks`](@ref).
"""
function pop_mark(mp::MarkedPoint{<:Any,<:Tuple})
    (MarkedPoint(mp.point, Base.tail(mp.mark)), mp.mark[1])
end
pop_mark(mp::MarkedPoint{<:Any,Tuple{}}) = (mp, nothing)

function pop_mark_type(::Type{MarkedPoint{E,T}}) where {E,T<:Tuple}
    MarkedPoint{E,Base.tuple_type_tail(T)}
end
pop_mark_type(::Type{M}) where {M<:MarkedPoint{<:Any,Tuple{}}} = M

function push_mark_type(::Type{NakedPoint{E}}, ::Type{M}) where {E,M}
    MarkedPoint{E,M}
end
function push_mark_type(::Type{MarkedPoint{E,N}}, ::Type{M}) where {E,N,M}
    MarkedPoint{E,Tuple{M,N}}
end
