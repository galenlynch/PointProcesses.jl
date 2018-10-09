# Must be well ordered, and support bounds and measure functions
abstract type Interval{E, N} end

measure(m::Interval) = measure(bounds(m))

check_overlap(m::Interval, n::Interval) = check_overlap(bounds(m)..., bounds(n)...)
nakedinterval(i::Interval) = NakedInterval(bounds(i))

function is_subinterval(m::Interval, parent::Interval)
    bm, em = bounds(m)
    bp, ep = bounds(parent)
    is_subinterval(bm, em, bp, ep)
end

struct NakedInterval{D<:Number} <: Interval{D, 1}
    interval::NTuple{2, D}
end
NakedInterval(a, b) = NakedInterval(promote(a, b))

bounds(m::NakedInterval) = m.interval

nakedinterval(m::NakedInterval) = m

struct MarkedInterval{D<:Number, M} <: Interval{D, 1}
    interval::NakedInterval{D}
    mark::M
end
MarkedInterval(t::NTuple{2, <:Number}, m) = MarkedInterval(NakedInterval(t), m)
MarkedInterval(a, b, m) = MarkedInterval(NakedInterval(a, b), m)

nakedinterval(m::MarkedInterval) = m.interval

bounds(m::MarkedInterval) = bounds(nakedinterval(m))

get_mark(m::MarkedInterval) = m.mark

_subinterval(m::MarkedInterval, s::NakedInterval) = MarkedInterval(s, get_mark(m))

struct RelativeInterval{D<:Number, I<:Interval{D}, J<:Interval{D}} <: Interval{D, 1}
    reference_interval::I
    anchored_left::Bool
    this_interval::J
end

measure(i::RelativeInterval) = measure(i.this_interval)

function bounds(i::RelativeInterval)
    r_bnds = bounds(i.reference_interval)
    tb, te = bounds(i.this_interval)
    bndno = ifelse(i.anchored_left, 1, 2)
    (tb + r_bnds[bndno], te + r_bnds[bndno])
end

# intervals must be contiguous
# They must also be ordered
# Cannot be empty
struct IntervalSet{E, T<:NTuple{<:Any, Interval{E, 1}}} <: Interval{E, 1}
    intervals::T
    function IntervalSet{E, T}(
        intervals::T
    ) where {E, T<:NTuple{<:Any, Interval{E}}}
        nint = length(intervals)
        length(nint) > 0 || throw(ArgumentError("Must not be empty"))
        last_e = bounds(intervals[1])[2]
        for i = 2:nint
            b, e = bounds(intervals[i])
            if b != last_e
                throw(ArgumentError("Intervals must be contiguous"))
            end
            last_e = e
        end
        new(intervals)
    end
end

function IntervalSet(intervals::T) where {E, T<:NTuple{<:Any, Interval{E, 1}}}
    IntervalSet{E, T}(intervals)
end
IntervalSet(i::Vararg{<:Interval}) = IntervalSet(i)

measure(i::IntervalSet) = sum(measure, i.intervals)

function bounds(i::IntervalSet{E, <:Any}) where E
    (bounds(i.intervals[1])[1], bounds(i.intervals[end])[2])::NTuple{2, E}
end

function complement!(
    out::AbstractVector{NakedInterval{E}},
    dom::Interval{E},
    int::Interval,
    pos::Integer = 1
) where E
    db, de = bounds(dom)
    ib, ie = bounds(int)
    if check_overlap(db, de, ib, ie)
        if ib > db
            out[pos] = NakedInterval(db, ib)
            if ie < de
                out[pos + 1] = NakedInterval(ie, de)
                nout = 2
            else
                nout = 1
            end
        elseif ie < de
            out[pos] = NakedInterval(ie, de)
            nout = 1
        else
            nout = 0
        end
    else
        out[pos] = dom
        nout = 1
    end
    nout
end

function complement(dom::Interval{E}, int::Interval) where E
    out = Vector{NakedInterval{E}}(undef, 2)
    i = complement!(out, dom, int)
    resize!(out, i)
end

function complement(dom::Interval{E}, ints::AbstractVector{<:Interval}) where E
    nint = length(ints)
    maxint = nint + 1
    out = Vector{NakedInterval{E}}(undef, maxint)
    outno = 0
    rem_dom = dom
    db, de = bounds(dom)
    for int in ints
        nout = complement!(out, rem_dom, int, outno + 1)
        outno += nout
        if nout > 0
            rem_dom = out[outno]
            if bounds(rem_dom)[2] == de
                outno -= 1
            else
                break
            end
        end
    end
    resize!(out, outno)
    out
end

# Assumes sorted by onset
function interval_levels(intervals::AbstractVector{<:Interval{E, 1}}) where E
    nint = length(intervals)
    end_heap = binary_minheap(E)
    out = Vector{MarkedInterval{E, Int}}(undef, 2 * (nint - 1) + 2)
    outno = 0
    level = 0
    for i = 1:(nint - 1)
        b, e = bounds(intervals[i])
        push!(end_heap, e)
        level += 1
        bn = bounds(intervals[i + 1])[1]
        if b < bn
            level, outno = cut_levels!(end_heap, out, b, bn, level, outno)
        end
    end
    b = bounds(intervals[end])[1]
    _, outno = cut_levels!(end_heap, out, b, typemax(E), level, outno)
    outno -= 1
    resize!(out, outno)
    out
end

function cut_levels!(end_heap, out, b, bn, level, outno)
    last_end = b
    while level > 0 && top(end_heap) <= bn
        outno += 1
        out[outno] = MarkedInterval(last_end, top(end_heap), level)
        ef = pop!(end_heap)
        level -= 1
        while level > 0 && top(end_heap) == ef
            pop!(end_heap)
            level -= 1
        end
        last_end = ef
    end
    outno += 1
    out[outno] = MarkedInterval(last_end, bn, level)
    level, outno
end
