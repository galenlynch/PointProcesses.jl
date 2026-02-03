"""
    Interval{E,N}

Abstract supertype for interval types, parameterised by the element type `E` and
dimensionality `N`. All concrete subtypes must implement [`bounds`](@ref) returning an
`NTuple{2,E}`.

Supports `in` membership testing, [`measure`](@ref), [`midpoint`](@ref),
[`bounds`](@ref), and [`nakedinterval`](@ref).
"""
abstract type Interval{E,N} end

function in(x, m::Interval)
    b, e = bounds(m)
    (x >= b) & (x <= e)
end

"""
    midpoint(m::Interval)

Return the midpoint of the interval `(b + e) / 2`.
"""
function midpoint(m::Interval)
    b, e = bounds(m)
    b + (e - b) / 2
end

"""
    measure(m::Interval)
    measure(m::IntervalSet)
    measure(p::Points)

Return the length (duration) of an interval. For an [`IntervalSet`](@ref), returns the
sum of the component measures. For a [`Points`](@ref) collection, returns the measure of
its underlying interval.
"""
measure(m::Interval) = measure(bounds(m))

"""
    subinterval(int::Interval{E,1}, b::Real, e::Real) -> Interval
    subinterval(int::Interval{E,1}, sub::NakedInterval{E}) -> Interval

Return the portion of `int` delimited by `[b, e]`. Preserves the type and mark of `int`
(e.g. a [`MarkedInterval`](@ref) yields a `MarkedInterval` with the same mark).

Throws `ArgumentError` if `[b, e]` is not contained within `int`.
"""
subinterval(int::Interval, b::Real, e::Real) = subinterval(int, NakedInterval(b, e))

"""
    interval_intersect(a::Interval, b::Interval) -> NakedInterval or nothing

Return the intersection of two intervals as a [`NakedInterval`](@ref), or `nothing` if
they do not overlap.
"""
function interval_intersect(a::Interval, b::Interval)
    int_int = interval_intersect(bounds(a), bounds(b))
    int_int === nothing ? nothing : NakedInterval(int_int)
end

"""
    interval_intersections(a::AbstractVector{<:Interval}, b::AbstractVector{<:Interval}) -> Vector{NakedInterval}

Compute all pairwise intersections between two sorted vectors of intervals. Both `a` and
`b` must be sorted by start time.
"""
function interval_intersections(
    a::AbstractVector{<:Interval},
    b::AbstractVector{<:Interval},
)
    NakedInterval.(interval_intersections(bounds.(a), bounds.(b)))
end

"""
    check_overlap(m::Interval, n::Interval) -> Bool

Return `true` if intervals `m` and `n` overlap (share at least one point).
"""
check_overlap(m::Interval, n::Interval) = check_overlap(bounds(m)..., bounds(n)...)

"""
    nakedinterval(i::Interval) -> NakedInterval
    nakedinterval(p::Points) -> NakedInterval

Strip any mark or wrapper to obtain a plain [`NakedInterval`](@ref). Identity on
`NakedInterval` inputs.
"""
nakedinterval(i::Interval) = NakedInterval(bounds(i))

"""
    is_subinterval(m::Interval, parent::Interval) -> Bool

Return `true` if `m` is entirely contained within `parent` (inclusive bounds).
"""
function is_subinterval(m::Interval, parent::Interval)
    bm, em = bounds(m)
    bp, ep = bounds(parent)
    is_subinterval(bm, em, bp, ep)
end

"""
    NakedInterval{D<:Number} <: Interval{D,1}

A plain interval storing `(start, stop)` bounds with no metadata. The fundamental
interval type in EventIntervals.

# Constructors

    NakedInterval((a, b))
    NakedInterval(a, b)

Arguments are promoted to a common numeric type.
"""
struct NakedInterval{D<:Number} <: Interval{D,1}
    interval::NTuple{2,D}
end
NakedInterval(a, b) = NakedInterval(promote(a, b))

"""
    bounds(m::Interval) -> NTuple{2,E}
    bounds(p::Points) -> NTuple{2,E}

Return the `(start, stop)` bounds of an interval or points collection.
"""
bounds(m::NakedInterval) = m.interval

nakedinterval(m::NakedInterval) = m

function subinterval(int::Interval{E,1}, sub::NakedInterval{E}) where {E}
    if ! is_subinterval(sub, int)
        throw(ArgumentError("Not a subinterval"))
    end
    _subinterval(int, sub)
end

_subinterval(::NakedInterval, sub::NakedInterval) = sub

"""
    MarkedInterval{D<:Number,M} <: Interval{D,1}

An interval with attached metadata of type `M`. The mark can be any type.

# Constructors

    MarkedInterval(interval::NakedInterval, mark)
    MarkedInterval((a, b), mark)
    MarkedInterval(a, b, mark)

See also [`get_mark`](@ref), [`nakedinterval`](@ref).
"""
struct MarkedInterval{D<:Number,M} <: Interval{D,1}
    interval::NakedInterval{D}
    mark::M
end
MarkedInterval(t::NTuple{2,<:Number}, m) = MarkedInterval(NakedInterval(t), m)
MarkedInterval(a, b, m) = MarkedInterval(NakedInterval(a, b), m)

nakedinterval(m::MarkedInterval) = m.interval

bounds(m::MarkedInterval) = bounds(nakedinterval(m))

"""
    get_mark(m::MarkedInterval)
    get_mark(iset::IntervalSet)
    get_mark(vp::VariablePoints)
    get_mark(sp::SubPoints)

Retrieve the mark from a marked object. For an [`IntervalSet`](@ref), returns the mark
of the first [`MarkedInterval`](@ref) component. For [`VariablePoints`](@ref) or
marked [`SubPoints`](@ref), returns the marks vector.

Throws an error if no mark is found.
"""
get_mark(m::MarkedInterval) = m.mark

_subinterval(m::MarkedInterval, s::NakedInterval) = MarkedInterval(s, get_mark(m))

"""
    RelativeInterval{D,I,J} <: Interval{D,1}

An interval expressed in coordinates relative to a reference interval. Useful for
defining peri-event windows (e.g. "100 ms before to 300 ms after stimulus onset").

    RelativeInterval(reference::Interval, anchored_left::Bool, offset::Interval)

If `anchored_left` is `true`, offsets are relative to the start of `reference`;
if `false`, relative to the end.

[`bounds`](@ref) resolves to absolute coordinates by adding the anchor point.
"""
struct RelativeInterval{D<:Number,I<:Interval{D},J<:Interval{D}} <: Interval{D,1}
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

function _subinterval(m::RelativeInterval, s::NakedInterval)
    rb, re = bounds(m.reference_interval)
    sb, se = bounds(s)
    anchor = ifelse(m.anchored_left, rb, re)
    new_int = _subinterval(m.this_interval, NakedInterval(sb - anchor, se - anchor))
    RelativeInterval(m.reference_interval, m.anchored_left, new_int)
end

"""
    IntervalSet{E,T} <: Interval{E,1}

A composite interval formed from a tuple of contiguous, ordered sub-intervals.
Behaves as a single interval whose bounds span from the start of the first component
to the end of the last.

# Constructors

    IntervalSet(i1::Interval, i2::Interval, ...)
    IntervalSet(intervals::AbstractVector{<:Interval})
    IntervalSet(intervals::NTuple{N,Interval})

Throws `ArgumentError` if the intervals are not contiguous (each must start where
the previous one ends) or if the collection is empty.

See also [`get_mark`](@ref).
"""
struct IntervalSet{E,T<:NTuple{<:Any,Interval{E,1}}} <: Interval{E,1}
    intervals::T
    function IntervalSet{E,T}(intervals::T) where {E,T<:NTuple{<:Any,Interval{E}}}
        nint = length(intervals)
        nint > 0 || throw(ArgumentError("Must not be empty"))
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

function IntervalSet(intervals::T) where {E,T<:NTuple{<:Any,Interval{E,1}}}
    IntervalSet{E,T}(intervals)
end
IntervalSet(i::Vararg{Interval}) = IntervalSet(i)
function IntervalSet(i::AbstractVector{<:Interval})
    isempty(i) && throw(ArgumentError("Must not be empty"))
    IntervalSet(Tuple(i))
end

measure(i::IntervalSet) = sum(measure, i.intervals)

function get_mark(i::IntervalSet)
    marked_int_no = findfirst(x -> isa(x, MarkedInterval), i.intervals)
    marked_int_no === nothing && error("Did not find a mark")
    get_mark(i.intervals[marked_int_no])
end

function bounds(i::IntervalSet{E,<:Any}) where {E}
    (bounds(i.intervals[1])[1], bounds(i.intervals[end])[2])::NTuple{2,E}
end

function _subinterval(m::IntervalSet{<:Any,T}, s::NakedInterval) where {T}
    overlap_idxs = findall(int -> check_overlap(int, s), m.intervals)
    noverlap = length(overlap_idxs)
    j_type = reduce(typejoin, T.parameters[overlap_idxs])
    new_ints = Vector{j_type}(undef, noverlap)
    for (i, int_idx) in enumerate(overlap_idxs)
        new_ints[i] =
            _subinterval(m.intervals[int_idx], interval_intersect(m.intervals[int_idx], s))
    end
    IntervalSet(Tuple(new_ints))
end

function complement!(
    out::AbstractVector{NakedInterval{E}},
    dom::Interval{E},
    int::Interval,
    pos::Integer = 1,
) where {E}
    db, de = bounds(dom)
    ib, ie = bounds(int)
    if check_overlap(db, de, ib, ie)
        if ib > db
            # Has to be in dom because there is overlap
            out[pos] = NakedInterval(db, ib)
            if ie < de
                out[pos+1] = NakedInterval(ie, de)
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
        out[pos] = nakedinterval(dom)
        nout = 1
    end
    nout
end

"""
    complement(dom::Interval, int::Interval) -> Vector{NakedInterval}
    complement(dom::Interval, ints::AbstractVector{<:Interval}) -> Vector{NakedInterval}

Return the portions of `dom` not covered by `int` (or by any interval in `ints`).
The vector form requires `ints` to be sorted by start time.
"""
function complement(dom::Interval{E}, int::Interval) where {E}
    out = Vector{NakedInterval{E}}(undef, 2)
    i = complement!(out, dom, int)
    resize!(out, i)
end

function complement(dom::Interval{E}, ints::AbstractVector{<:Interval}) where {E}
    out = NakedInterval{E}[]
    if isempty(ints)
        push!(out, nakedinterval(dom))
        return out
    end
    rem_dom = dom
    for int in ints
        db_r, de_r = bounds(rem_dom)
        ib, ie = bounds(int)
        if check_overlap(db_r, de_r, ib, ie)
            if ib > db_r
                push!(out, NakedInterval(db_r, ib))
            end
            if ie < de_r
                rem_dom = NakedInterval(ie, de_r)
            else
                return out
            end
        end
    end
    push!(out, nakedinterval(rem_dom))
    out
end

function cut_levels!(end_heap, out, b, bn, level)
    last_end = b
    while level > 0 && first(end_heap) <= bn
        ef = pop!(end_heap)
        push!(out, MarkedInterval(last_end, ef, level))
        level -= 1
        while level > 0 && first(end_heap) == ef
            pop!(end_heap)
            level -= 1
        end
        last_end = ef
    end
    push!(out, MarkedInterval(last_end, bn, level))
    level
end

"""
    interval_levels(intervals::AbstractVector{<:Interval{E,1}}) where E -> Vector{MarkedInterval{E,Int}}

Partition the time axis into segments labelled by the depth of interval overlap. Returns
a vector of [`MarkedInterval`](@ref) where each mark is the number of input intervals
overlapping that segment.

`intervals` must be sorted by start time and non-empty (throws `ArgumentError` otherwise).
"""
function interval_levels(intervals::AbstractVector{<:Interval{E,1}}) where {E}
    nint = length(intervals)
    nint > 0 || throw(ArgumentError("intervals must not be empty"))
    end_heap = BinaryMinHeap{E}()
    out = MarkedInterval{E,Int}[]
    level = 0
    for i = 1:(nint-1)
        b, e = bounds(intervals[i])
        push!(end_heap, e)
        level += 1
        bn = bounds(intervals[i+1])[1]
        if b < bn
            level = cut_levels!(end_heap, out, b, bn, level)
        end
    end
    b, e_last = bounds(intervals[end])
    push!(end_heap, e_last)
    level += 1
    cut_levels!(end_heap, out, b, typemax(E), level)
    pop!(out)  # remove trailing sentinel segment to typemax
    out
end

"""
    chunk(int::Interval{E,1}, chunk_len::E, exact::Bool=false) where E -> Vector{NakedInterval{E}}

Break `int` into consecutive sub-intervals of length `chunk_len`. If `exact` is `true`,
any trailing remainder shorter than `chunk_len` is dropped; otherwise it is included as
the final chunk.

`chunk_len` must be positive (throws `ArgumentError` otherwise).

    chunk(intervals::AbstractVector{<:Interval{E,1}}, chunk_len::E, exact::Bool=true) where E

Apply chunking to each interval in the vector and concatenate the results.
"""
function chunk(int::Interval{E,1}, chunk_len::E, exact::Bool = false) where {E}
    chunk_len > 0 || throw(ArgumentError("chunk_len must be positive"))
    m = measure(int)
    b, e = bounds(int)
    nc = convert(Int, ifelse(exact, fld(m, chunk_len), cld(m, chunk_len)))
    out = Vector{NakedInterval{E}}(undef, nc)
    for i = 1:nc
        nb = b + chunk_len * (i - 1)
        ne = min(b + chunk_len * i, e)
        out[i] = _subinterval(int, NakedInterval(nb, ne))
    end
    out
end

function chunk(intervals::AbstractVector{<:Interval{E,1}}, chunk_len::E, exact::Bool=true) where E
    isempty(intervals) && return NakedInterval{E}[]
    total_dur = mapreduce(measure, +, intervals, init=zero(E))
    div_fn = exact ? fld : cld
    max_n_chunk = convert(Int, div_fn(total_dur, chunk_len))
    outs = Vector{NakedInterval{E}}(undef, max_n_chunk)
    nout = 0
    for int in intervals
        chunks = chunk(int, chunk_len, exact)
        nchunk = length(chunks)
        outs[nout+1 : nout+nchunk] = chunks
        nout += nchunk
    end
    clipsize!(outs, nout)
    outs
end

"""
    relative_interval(interval, reference) -> NakedInterval

Express `interval` in coordinates relative to `reference` by subtracting the reference
start from both bounds. Either argument may be an [`Interval`](@ref) or an
`NTuple{2,<:Number}`.
"""
function relative_interval(interval::Interval, reference::Interval)
    NakedInterval(relative_interval(bounds(interval), bounds(reference)))
end
function relative_interval(interval::Interval, reference::NTuple{2,<:Number})
    NakedInterval(relative_interval(bounds(interval), reference))
end
function relative_interval(interval::NTuple{2,<:Number}, reference::Interval)
    NakedInterval(relative_interval(interval, bounds(reference)))
end

"""
    shrink(int::Interval, shrink_measure) -> Interval
    shrink(ints::AbstractVector{<:Interval}, shrink_measure) -> Vector

Contract an interval inward by `shrink_measure / 2` from each end. If `shrink_measure`
exceeds the interval's measure, the result is a zero-width interval at the midpoint.

The vector form drops any intervals whose measure is less than `shrink_measure` before
shrinking the survivors.
"""
function shrink(int::Interval, shrink_measure)
    m = measure(int)
    adj_shrink = min(shrink_measure, m) / 2
    b, e = bounds(int)
    subinterval(int, b + adj_shrink, e - adj_shrink)
end

function shrink(ints::AbstractVector{<:Interval}, shrink_measure)
    survivors = findall(i -> measure(i) >= shrink_measure, ints)
    nout = length(survivors)
    out = similar(ints, nout)
    adj_shrink = shrink_measure / 2
    for (outno, intno) in enumerate(survivors)
        b, e = bounds(ints[intno])
        out[outno] =
            _subinterval(ints[intno], NakedInterval(b + adj_shrink, e - adj_shrink))
    end
    out
end

"""
    mask_events(evts, i::Interval)

Filter a sorted event vector to only those falling within interval `i`. Delegates to
`SortedIntervals.mask_events(evts, b, e)`.
"""
function mask_events(evts, i::Interval)
    b, e = bounds(i)
    mask_events(evts, b, e)
end

"""
    interval_indices(basis, i::Interval)
    interval_indices(basis, a::AbstractVector{<:Interval})

Return the indices into `basis` that fall within interval `i`. The vector form maps
over each interval independently.
"""
function interval_indices(basis, i::Interval)
    b, e = bounds(i)
    interval_indices(basis, b, e)
end

function interval_indices(basis, a::AbstractVector{<:Interval})
    map(i -> interval_indices(basis, i), a)
end

"""
    intervals_diff(a::AbstractVector{<:Interval}, b) -> Vector{NakedInterval}
    intervals_diff(a, b::AbstractVector{<:Interval}) -> Vector{NakedInterval}

Compute the set difference of two sorted interval vectors: the portions of `a` not
covered by `b`. Accepts [`Interval`](@ref) objects or `NTuple{2}` bounds in either
argument position.
"""
function intervals_diff(a_ints::AbstractVector{<:Interval}, b_ints)
    NakedInterval.(intervals_diff(map(i -> bounds(i), a_ints), b_ints))
end

function intervals_diff(a_ints, b_ints::AbstractVector{<:Interval})
    NakedInterval.(intervals_diff(a_ints, map(i -> bounds(i), b_ints)))
end

"""
    maximum_interval_overlap(xs::AbstractVector{<:Interval}, y::Interval) -> Number

Return the maximum overlap (in measure) between any single interval in `xs` and the
interval `y`.
"""
function maximum_interval_overlap(xs::AbstractVector{<:Interval}, y::Interval)
    maximum_interval_overlap(bounds.(xs), bounds(y))
end

"""
    shift_interval(a::Interval, offset::Number) -> Interval
    shift_interval(offset::Number) -> Function

Translate an interval by adding `offset` to both bounds. Preserves the mark on a
[`MarkedInterval`](@ref). The single-argument form returns a curried function.
"""
shift_interval(a::NakedInterval, offset::Number) = NakedInterval(bounds(a) .+ offset)
shift_interval(a::MarkedInterval, offset::Number) =
    MarkedInterval(bounds(a) .+ offset, get_mark(a))

shift_interval(offset::Number) = (a::Interval) -> shift_interval(a, offset)
