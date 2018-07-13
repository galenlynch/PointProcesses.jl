abstract type Point{E} end # must have point field

struct NakedPoint{E} <: Point{E}
    point::E
end

point_values(pts::AbstractVector{<:NakedPoint}) = getfield.(pts, :point)

struct MarkedPoint{E, M} <: Point{E}
    point::E
    mark::M
end

function point_values(pts::AbstractVector{MarkedPoint{E, M}}) where {E, M}
    np = length(pts)
    pnt_vals = Vector{E}(np)
    marks = Vector{M}(np)
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

function pop_mark_type(
    ::Type{MarkedPoint{E, T}}
) where {E, T<:Tuple{<:Any, Vararg}}
    MarkedPoint{E, Tuple{T.parameters[2:end]...}}
end
pop_mark_type(::Type{M}) where M<:MarkedPoint{<:Any, Tuple{}} = M

function push_mark_type(::Type{NakedPoint{E}}, ::Type{M}) where {E, M}
    MarkedPoint{E, M}
end
function push_mark_type(::Type{MarkedPoint{E, N}}, ::Type{M}) where {E, N, M}
    MarkedPoint{E, Tuple{M, N}}
end
