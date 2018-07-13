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

prepend_mark(mp::MarkedPoint, newmark) = MarkedPoint(mp.point, (newmark, mp.mark))
prepend_mark(p::NakedPoint, mark) = MarkedPoint(p.point, mark)

function prepend_mark_type(::Type{NakedPoint{E}}, ::Type{M}) where {E, M}
    MarkedPoint{E, M}
end
function prepend_mark_type(::Type{MarkedPoint{E, N}}, ::Type{M}) where {E, N, M}
    MarkedPoint{E, Tuple{M, N}}
end
