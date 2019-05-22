abstract type Point{E} end # must have point field

isless(pt::Point, n::Number) = isless(nakedpointvalue(pt), n)
isless(n::Number, pt::Point) = isless(n, nakedpointvalue(pt))

struct NakedPoint{E} <: Point{E}
    point::E
end

function show(io::IO, pt::T) where T<:NakedPoint
    get(io, :typeinfo, nothing) != T && print(io, T)
    print(io, pt.point)
end

nakedpointvalue(pt::NakedPoint) = pt.point

point_values(pts::AbstractVector{<:NakedPoint}) = getfield.(pts, :point)

struct MarkedPoint{E, M} <: Point{E}
    point::E
    mark::M
end

function show(io::IO, pt::T) where T<:MarkedPoint
    get(io, :typeinfo, nothing) != T && print(io, T)
    print(io, '(', pt.point, ", ", pt.mark, ')')
end

function point_values(pts::AbstractVector{MarkedPoint{E, M}}) where {E, M}
    np = length(pts)
    @compat pnt_vals = Vector{E}(undef, np)
    @compat marks = Vector{M}(undef, np)
    @inbounds for (i, p) in enumerate(pts)
        pnt_vals[i] = p.point
        marks[i] = p.mark
    end
    pnt_vals, marks
end

push_mark(mp::MarkedPoint, newmark) = MarkedPoint(mp.point, (newmark, mp.mark))
push_mark(p::NakedPoint, mark) = MarkedPoint(p.point, mark)

function pop_mark(mp::MarkedPoint{<:Any, <:Tuple})
    (MarkedPoint(mp.point, Base.tail(mp.mark)), mp.mark[1])
end
pop_mark(mp::MarkedPoint{<:Any, Tuple{}}) = (mp, nothing)

function pop_mark_type(::Type{MarkedPoint{E, T}}) where {E, T<:Tuple}
    MarkedPoint{E, Base.tuple_type_tail(T)}
end
pop_mark_type(::Type{M}) where M<:MarkedPoint{<:Any, Tuple{}} = M

function push_mark_type(::Type{NakedPoint{E}}, ::Type{M}) where {E, M}
    MarkedPoint{E, M}
end
function push_mark_type(::Type{MarkedPoint{E, N}}, ::Type{M}) where {E, N, M}
    MarkedPoint{E, Tuple{M, N}}
end
