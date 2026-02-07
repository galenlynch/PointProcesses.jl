using EventIntervals
using Random
using Test

const srand = Random.seed!

@testset "EventIntervals" begin
    pp_testarr = [1, 2, 3]
    pp_notshuff_arr = [2, 1, 3]
    interval = (0, 5)
    NakedPoints(pp_notshuff_arr)
    NakedPoints(pp_testarr, interval[1], interval[2])
    NakedPoints(pp_testarr)
    spp = NakedPoints(pp_testarr, interval)
    @test duration(spp) == 5
    @test count(spp) == 3
    @test count(spp, 2, 5) == 2
    @test point_values(spp) == [1, 2, 3]
    @test all(point_values(spp, 0, 1) .== [1])

    mag = [2, 4, 6]
    VariablePoints(spp, mag)
    mpp = VariablePoints(pp_testarr, mag, interval)
    @test duration(mpp) == 5
    @test count(mpp) == 3
    @test count(mpp, 2, 5) == 2
    @test point_values(mpp) == (pp_testarr, mag)
    p, m = point_values(mpp, 2, 8)
    @test all(p .== [2, 3])
    @test all(m .== [4, 6])

    spp = SubPoints(spp, (2, 5))
    spp = SubPoints(spp, 2, 5)
    @test duration(spp) == 3
    @test count(spp) == 2
    @test count(spp, 1, 5) == 2
    @test all(point_values(spp, 1, 5) .== [2, 3])

    srand(1)
    big_A = rand(20)
    pp_big = NakedPoints(big_A, 0, 1)
    pp_downsamp(pp_big, 0, 1, 0.05)
    pp_downsamp(
        pp_big,
        0,
        1,
        0.05,
        pt_extent_merge,
        MarkedPoint{Float64,Tuple{Float64,Float64}},
    )
    @testset "chunk vector" begin
        ints = [NakedInterval(0.0, 3.0), NakedInterval(5.0, 8.0)]
        chunks = chunk(ints, 1.0, true)
        @test length(chunks) == 6
        @test all(c -> measure(c) ≈ 1.0, chunks)
        # empty input
        @test chunk(NakedInterval{Float64}[], 1.0) == NakedInterval{Float64}[]
        # exact drops remainder
        chunks_exact = chunk([NakedInterval(0.0, 2.5)], 1.0, true)
        @test length(chunks_exact) == 2
        # inexact: remainder included
        chunks_inexact = chunk([NakedInterval(0.0, 2.5)], 1.0, false)
        @test length(chunks_inexact) == 3
    end

    @testset "complement multi-interval" begin
        dom = NakedInterval((0.0, 10.0))
        # trailing remainder must be included
        r = complement(dom, [NakedInterval((3.0, 5.0)), NakedInterval((7.0, 9.0))])
        @test bounds.(r) == [(0.0, 3.0), (5.0, 7.0), (9.0, 10.0)]
        # single interval at the start
        r = complement(dom, [NakedInterval((0.0, 3.0))])
        @test bounds.(r) == [(3.0, 10.0)]
        # single interval in the middle
        r = complement(dom, [NakedInterval((3.0, 7.0))])
        @test bounds.(r) == [(0.0, 3.0), (7.0, 10.0)]
        # full coverage
        r = complement(dom, [NakedInterval((0.0, 10.0))])
        @test isempty(r)
        # single interval at the end
        r = complement(dom, [NakedInterval((5.0, 10.0))])
        @test bounds.(r) == [(0.0, 5.0)]
        # three intervals
        r = complement(dom, [NakedInterval((1.0, 2.0)), NakedInterval((4.0, 6.0)), NakedInterval((8.0, 9.0))])
        @test bounds.(r) == [(0.0, 1.0), (2.0, 4.0), (6.0, 8.0), (9.0, 10.0)]
        # empty interval vector
        r = complement(dom, NakedInterval{Float64}[])
        @test bounds.(r) == [(0.0, 10.0)]
    end

    @testset "interval_levels" begin
        # two overlapping intervals
        levels = interval_levels([NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0))])
        @test bounds.(levels) == [(0.0, 3.0), (3.0, 5.0), (5.0, 8.0)]
        @test get_mark.(levels) == [1, 2, 1]
        # three overlapping intervals
        levels = interval_levels([NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0)), NakedInterval((6.0, 10.0))])
        @test bounds.(levels) == [(0.0, 3.0), (3.0, 5.0), (5.0, 6.0), (6.0, 8.0), (8.0, 10.0)]
        @test get_mark.(levels) == [1, 2, 1, 2, 1]
        # non-overlapping intervals
        levels = interval_levels([NakedInterval((0.0, 2.0)), NakedInterval((5.0, 7.0))])
        @test bounds.(levels) == [(0.0, 2.0), (2.0, 5.0), (5.0, 7.0)]
        @test get_mark.(levels) == [1, 0, 1]
    end

    @testset "VariablePoints unsorted construction" begin
        vp = VariablePoints([3.0, 1.0, 2.0], [:c, :a, :b], (0.0, 5.0))
        @test vp[1] == MarkedPoint(1.0, :a)
        @test vp[2] == MarkedPoint(2.0, :b)
        @test vp[3] == MarkedPoint(3.0, :c)
    end

    @testset "setindex! read-only" begin
        pts = NakedPoints([1.0, 2.0, 3.0], (0.0, 5.0))
        @test_throws ReadOnlyMemoryError pts[1] = NakedPoint(0.5)
    end

    @testset "SubPoints point_values non-overlapping range" begin
        pts = NakedPoints([1.0, 2.0, 3.0], NakedInterval((0.0, 5.0)))
        sp = SubPoints(pts, NakedInterval((1.0, 3.0)))
        # range outside the subpoints interval should not error
        r = point_values(sp, 10.0, 20.0)
        @test isempty(r)
    end

    @testset "complement with MarkedInterval domain" begin
        dom = MarkedInterval((0.0, 10.0), :session)
        # single interval, no overlap
        r = complement(dom, NakedInterval((15.0, 20.0)))
        @test bounds.(r) == [(0.0, 10.0)]
        # single interval, partial overlap
        r = complement(dom, NakedInterval((3.0, 7.0)))
        @test bounds.(r) == [(0.0, 3.0), (7.0, 10.0)]
        # multi-interval with MarkedInterval domain
        r = complement(dom, [NakedInterval((2.0, 4.0)), NakedInterval((6.0, 8.0))])
        @test bounds.(r) == [(0.0, 2.0), (4.0, 6.0), (8.0, 10.0)]
    end

    @testset "interval_levels empty input" begin
        @test_throws ArgumentError interval_levels(NakedInterval{Float64}[])
    end

    @testset "SubPoints getindex O(1)" begin
        pts = NakedPoints(collect(1.0:1000.0), NakedInterval((0.0, 1001.0)))
        sp = SubPoints(pts, NakedInterval((500.0, 600.0)))
        # verify correct indexing
        @test sp[1] == NakedPoint(500.0)
        @test sp[count(sp)] == NakedPoint(600.0)
        # verify iteration works
        vals = [nakedpointvalue(p) for p in sp]
        @test vals == collect(500.0:600.0)
    end

    @testset "interval_intersections_subpoints buffer" begin
        # two point collections, two intervals, with 3 intersections
        pts1 = NakedPoints([0.5, 1.0, 2.0], NakedInterval((0.0, 3.0)))
        pts2 = NakedPoints([4.5, 5.0, 6.0], NakedInterval((4.0, 7.0)))
        # interval that spans across both point collections
        ints = [NakedInterval((0.5, 5.5))]
        subs = interval_intersections_subpoints([pts1, pts2], ints)
        @test length(subs) == 2
        @test bounds(subs[1]) == (0.5, 3.0)
        @test bounds(subs[2]) == (4.0, 5.5)
    end

    @testset "relative_interval Interval" begin
        ref = NakedInterval(5.0, 15.0)
        int = NakedInterval(7.0, 12.0)
        ri = relative_interval(int, ref)
        @test bounds(ri) == (2.0, 7.0)
        # tuple/Interval mixed dispatch
        ri2 = relative_interval(int, (5.0, 15.0))
        @test bounds(ri2) == (2.0, 7.0)
        ri3 = relative_interval((7.0, 12.0), ref)
        @test bounds(ri3) == (2.0, 7.0)
    end

    # ================================================================
    # NEW TESTS
    # ================================================================

    # --------------------------------------------------
    # 1. Interval types and properties
    # --------------------------------------------------
    @testset "NakedInterval" begin
        ni = NakedInterval((2.0, 5.0))
        @test bounds(ni) == (2.0, 5.0)
        @test measure(ni) == 3.0
        @test midpoint(ni) == 3.5
        @test 3.0 in ni
        @test !(6.0 in ni)
        # two-arg constructor
        ni2 = NakedInterval(1.0, 4.0)
        @test bounds(ni2) == (1.0, 4.0)
        # nakedinterval identity
        @test nakedinterval(ni) === ni
        # promotion across types
        ni3 = NakedInterval(1, 4.0)
        @test bounds(ni3) == (1.0, 4.0)
    end

    @testset "MarkedInterval" begin
        mi = MarkedInterval((1.0, 3.0), :tag)
        @test bounds(mi) == (1.0, 3.0)
        @test measure(mi) == 2.0
        @test get_mark(mi) == :tag
        # nakedinterval strips mark
        @test nakedinterval(mi) == NakedInterval(1.0, 3.0)
        @test nakedinterval(mi) isa NakedInterval
        # three-arg constructor
        mi2 = MarkedInterval(2.0, 6.0, 42)
        @test bounds(mi2) == (2.0, 6.0)
        @test get_mark(mi2) == 42
    end

    @testset "RelativeInterval" begin
        ref = NakedInterval(10.0, 20.0)
        rel = NakedInterval(1.0, 3.0)
        # left-anchored
        ri = RelativeInterval(ref, true, rel)
        @test bounds(ri) == (11.0, 13.0)
        @test measure(ri) == 2.0
        # right-anchored
        ri2 = RelativeInterval(ref, false, rel)
        @test bounds(ri2) == (21.0, 23.0)
    end

    @testset "IntervalSet" begin
        i1 = NakedInterval(0.0, 3.0)
        i2 = NakedInterval(3.0, 5.0)
        # vararg construction
        iset = IntervalSet(i1, i2)
        @test bounds(iset) == (0.0, 5.0)
        @test measure(iset) == 5.0
        # vector construction
        iset2 = IntervalSet([i1, i2])
        @test bounds(iset2) == (0.0, 5.0)
        # tuple construction
        iset3 = IntervalSet((i1, i2))
        @test bounds(iset3) == (0.0, 5.0)
        # get_mark finds first marked
        mi = MarkedInterval(3.0, 5.0, :found)
        iset_marked = IntervalSet(i1, mi)
        @test get_mark(iset_marked) == :found
        # contiguity validation error
        bad1 = NakedInterval(0.0, 2.0)
        bad2 = NakedInterval(3.0, 5.0)  # gap at 2→3
        @test_throws ArgumentError IntervalSet(bad1, bad2)
        # empty not allowed
        @test_throws ArgumentError IntervalSet(NakedInterval{Float64}[])
    end

    # --------------------------------------------------
    # 2. Interval operations
    # --------------------------------------------------
    @testset "subinterval" begin
        parent = NakedInterval(0.0, 10.0)
        sub = subinterval(parent, 2.0, 8.0)
        @test bounds(sub) == (2.0, 8.0)
        # rejects non-subinterval
        @test_throws ArgumentError subinterval(parent, -1.0, 5.0)
        # preserves MarkedInterval mark
        mparent = MarkedInterval(0.0, 10.0, :keep)
        msub = subinterval(mparent, 2.0, 8.0)
        @test bounds(msub) == (2.0, 8.0)
        @test get_mark(msub) == :keep
    end

    @testset "interval_intersect" begin
        a = NakedInterval(0.0, 5.0)
        b = NakedInterval(3.0, 8.0)
        r = interval_intersect(a, b)
        @test bounds(r) == (3.0, 5.0)
        # non-overlapping returns nothing
        c = NakedInterval(6.0, 8.0)
        @test interval_intersect(a, c) === nothing
        # partial overlap
        d = NakedInterval(4.0, 12.0)
        r2 = interval_intersect(a, d)
        @test bounds(r2) == (4.0, 5.0)
    end

    @testset "interval_intersections" begin
        as = [NakedInterval(0.0, 3.0), NakedInterval(5.0, 8.0)]
        bs = [NakedInterval(2.0, 6.0), NakedInterval(7.0, 10.0)]
        r = interval_intersections(as, bs)
        @test length(r) == 3
        @test bounds(r[1]) == (2.0, 3.0)
        @test bounds(r[2]) == (5.0, 6.0)
        @test bounds(r[3]) == (7.0, 8.0)
    end

    @testset "check_overlap" begin
        a = NakedInterval(0.0, 5.0)
        b = NakedInterval(3.0, 8.0)
        @test check_overlap(a, b) == true
        # touching at boundary
        c = NakedInterval(5.0, 8.0)
        @test check_overlap(a, c) == true
        # non-overlapping
        d = NakedInterval(6.0, 8.0)
        @test check_overlap(a, d) == false
    end

    @testset "is_subinterval" begin
        parent = NakedInterval(0.0, 10.0)
        child = NakedInterval(2.0, 8.0)
        @test is_subinterval(child, parent) == true
        # not contained
        outside = NakedInterval(-1.0, 5.0)
        @test is_subinterval(outside, parent) == false
        # equal
        @test is_subinterval(parent, parent) == true
    end

    @testset "shrink" begin
        int = NakedInterval(0.0, 10.0)
        s = shrink(int, 2.0)
        @test bounds(s) == (1.0, 9.0)
        @test measure(s) == 8.0
        # vector shrink drops too-small intervals
        ints = [NakedInterval(0.0, 10.0), NakedInterval(12.0, 13.0)]
        sv = shrink(ints, 2.0)
        @test length(sv) == 1
        @test bounds(sv[1]) == (1.0, 9.0)
        # shrink to zero-width (shrink_measure >= measure)
        s0 = shrink(int, 20.0)
        @test measure(s0) ≈ 0.0 atol=1e-12
    end

    @testset "shift_interval" begin
        ni = NakedInterval(1.0, 4.0)
        shifted = shift_interval(ni, 3.0)
        @test bounds(shifted) == (4.0, 7.0)
        # MarkedInterval preserves mark
        mi = MarkedInterval(1.0, 4.0, :m)
        shifted_m = shift_interval(mi, 3.0)
        @test bounds(shifted_m) == (4.0, 7.0)
        @test get_mark(shifted_m) == :m
        # curried form
        shifter = shift_interval(5.0)
        @test bounds(shifter(ni)) == (6.0, 9.0)
    end

    @testset "mask_events Interval dispatch" begin
        evts = [1.0, 3.0, 5.0, 7.0]
        int = NakedInterval(2.0, 6.0)
        masked = mask_events(evts, int)
        @test masked == [3.0, 5.0]
    end

    @testset "intervals_diff Interval dispatch" begin
        a_ints = [NakedInterval(0.0, 5.0), NakedInterval(6.0, 10.0)]
        b_ints = [NakedInterval(3.0, 7.0)]
        # first argument position
        r1 = intervals_diff(a_ints, [(3.0, 7.0)])
        @test length(r1) == 2
        @test bounds(r1[1]) == (0.0, 3.0)
        @test bounds(r1[2]) == (7.0, 10.0)
        # second argument position
        r2 = intervals_diff([(0.0, 5.0), (6.0, 10.0)], b_ints)
        @test length(r2) == 2
        @test bounds(r2[1]) == (0.0, 3.0)
        @test bounds(r2[2]) == (7.0, 10.0)
    end

    @testset "midpoint" begin
        @test midpoint(NakedInterval(0.0, 10.0)) == 5.0
        @test midpoint(MarkedInterval(2.0, 6.0, :x)) == 4.0
    end

    # --------------------------------------------------
    # 3. Point types
    # --------------------------------------------------
    @testset "NakedPoint" begin
        np = NakedPoint(3.5)
        @test nakedpointvalue(np) == 3.5
        @test np < 4.0
        @test !(np < 3.0)
        @test 2.0 < np
    end

    @testset "MarkedPoint" begin
        mp = MarkedPoint(2.0, :tag)
        @test nakedpointvalue(mp) == 2.0
        @test mp < 3.0
        @test 1.0 < mp
    end

    @testset "push_mark" begin
        np = NakedPoint(1.0)
        mp = push_mark(np, :a)
        @test mp == MarkedPoint(1.0, :a)
        # on MarkedPoint nests tuple
        mp2 = push_mark(mp, :b)
        @test mp2 == MarkedPoint(1.0, (:b, :a))
    end

    @testset "pop_mark" begin
        mp = MarkedPoint(1.0, (:outer, :inner))
        popped, mark = pop_mark(mp)
        @test mark == :outer
        @test popped == MarkedPoint(1.0, (:inner,))
        # empty tuple mark
        mp_empty = MarkedPoint(1.0, ())
        popped2, mark2 = pop_mark(mp_empty)
        @test mark2 === nothing
        @test popped2 === mp_empty
    end

    # --------------------------------------------------
    # 4. Points collections
    # --------------------------------------------------
    @testset "NakedPoints constructors" begin
        # vector + interval object
        np1 = NakedPoints([1.0, 2.0, 3.0], NakedInterval(0.0, 5.0))
        @test count(np1) == 3
        @test bounds(np1) == (0.0, 5.0)
        # vector + tuple
        np2 = NakedPoints([1.0, 2.0], (0.0, 5.0))
        @test count(np2) == 2
        # vector + a + b
        np3 = NakedPoints([1.0, 2.0], 0.0, 5.0)
        @test count(np3) == 2
        # vector only (infers interval)
        np4 = NakedPoints([3.0, 1.0, 2.0])
        @test point_values(np4) == [1.0, 2.0, 3.0]
        @test bounds(np4) == (1.0, 3.0)
        # NakedPoint vector (with Interval)
        np5 = NakedPoints([NakedPoint(2.0), NakedPoint(1.0)], NakedInterval(0.0, 5.0))
        @test point_values(np5) == [1.0, 2.0]
        # NakedPoint vector (with tuple — previously ambiguous)
        np6 = NakedPoints([NakedPoint(2.0), NakedPoint(1.0)], (0.0, 5.0))
        @test point_values(np6) == [1.0, 2.0]
        # empty points on interval
        np_empty = NakedPoints(Float64[], (0.0, 5.0))
        @test count(np_empty) == 0
        # interval accessor
        @test EventIntervals.interval(np1) == NakedInterval(0.0, 5.0)
        # nakedpoints identity
        @test nakedpoints(np1) === np1
        # rate
        @test rate(np1) == 3 / 5
        # time_interval
        @test time_interval(np1) == EventIntervals.interval(np1)
    end

    @testset "VariablePoints" begin
        nk = NakedPoints([1.0, 2.0, 3.0], (0.0, 5.0))
        vp = VariablePoints(nk, [10, 20, 30])
        @test count(vp) == 3
        @test get_mark(vp) == [10, 20, 30]
        # from MarkedPoint vector
        mpts = [MarkedPoint(3.0, :c), MarkedPoint(1.0, :a), MarkedPoint(2.0, :b)]
        vp2 = VariablePoints(mpts, (0.0, 5.0))
        @test vp2[1] == MarkedPoint(1.0, :a)
        @test vp2[3] == MarkedPoint(3.0, :c)
        # translate
        vp_t = translate(vp, 10.0)
        @test bounds(vp_t) == (10.0, 15.0)
        pv, mv = point_values(vp_t)
        @test pv == [11.0, 12.0, 13.0]
        @test mv == [10, 20, 30]
    end

    @testset "SubPoints" begin
        pts = NakedPoints([1.0, 2.0, 3.0, 4.0], NakedInterval(0.0, 5.0))
        # maybe_subpoints overlap
        sp = maybe_subpoints(pts, NakedInterval(1.5, 3.5))
        @test sp !== nothing
        @test count(sp) == 2
        @test bounds(sp) == (1.5, 3.5)
        # maybe_subpoints no overlap
        sp_none = maybe_subpoints(pts, NakedInterval(6.0, 8.0))
        @test sp_none === nothing
        # nested SubPoints flattening
        sp1 = SubPoints(pts, NakedInterval(1.0, 4.0))
        sp2 = SubPoints(sp1, NakedInterval(2.0, 3.0))
        @test count(sp2) == 2
        @test bounds(sp2) == (2.0, 3.0)
        # nakedpoints extraction
        np_ex = nakedpoints(sp1)
        @test np_ex isa NakedPoints
        @test bounds(np_ex) == (1.0, 4.0)
        # VariablePoints-backed SubPoints
        vp = VariablePoints([1.0, 2.0, 3.0, 4.0], [:a, :b, :c, :d], (0.0, 5.0))
        sp_vp = SubPoints(vp, NakedInterval(1.5, 3.5))
        @test count(sp_vp) == 2
        pv, mv = point_values(sp_vp)
        @test collect(pv) == [2.0, 3.0]
        @test collect(mv) == [:b, :c]
    end

    @testset "points" begin
        nk = NakedPoints([1.0, 2.0], (0.0, 5.0))
        ps = points(nk)
        @test ps == [NakedPoint(1.0), NakedPoint(2.0)]
        # VariablePoints
        vp = VariablePoints(nk, [:a, :b])
        ps2 = points(vp)
        @test ps2 == [MarkedPoint(1.0, :a), MarkedPoint(2.0, :b)]
    end

    @testset "nakedvalues" begin
        nk = NakedPoints([1.0, 2.0, 3.0, 4.0], (0.0, 5.0))
        # full range
        @test collect(nakedvalues(nk)) == [1.0, 2.0, 3.0, 4.0]
        # sub-range
        @test collect(nakedvalues(nk, 1.5, 3.5)) == [2.0, 3.0]
    end

    # --------------------------------------------------
    # 5. Points operations
    # --------------------------------------------------
    @testset "rate" begin
        nk = NakedPoints([1.0, 2.0, 3.0], (0.0, 5.0))
        @test rate(nk) == 3 / 5
        # SubPoints rate
        sp = SubPoints(nk, NakedInterval(1.0, 3.0))
        @test rate(sp) == 3 / 2  # 3 points (1,2,3) in interval of measure 2
        # vector of Points
        nk2 = NakedPoints([6.0, 7.0], (5.0, 10.0))
        @test rate([nk, nk2]) == 5 / 10
        # empty vector
        empty_pts = typeof(nk)[]
        @test rate(empty_pts) === nothing
    end

    @testset "join_points" begin
        nk1 = NakedPoints([1.0, 2.0], (0.0, 3.0))
        nk2 = NakedPoints([2.5, 4.0], (2.0, 5.0))
        joined = join_points(nk1, nk2)
        @test joined isa NakedPoints
        @test count(joined) == 4
        @test point_values(joined) == [1.0, 2.0, 2.5, 4.0]
        # VariablePoints join
        vp1 = VariablePoints(nk1, [:a, :b])
        vp2 = VariablePoints(nk2, [:c, :d])
        vjoin = join_points(vp1, vp2)
        @test vjoin isa VariablePoints
        @test count(vjoin) == 4
        pv, mv = point_values(vjoin)
        @test pv == [1.0, 2.0, 2.5, 4.0]
        @test mv == [:a, :b, :c, :d]
        # vararg (3+ points)
        nk3 = NakedPoints([5.5], (5.0, 6.0))
        joined3 = join_points(nk1, nk2, nk3)
        @test count(joined3) == 5
        # single Points materialization
        sp = SubPoints(nk1, NakedInterval(0.5, 2.5))
        mat = join_points(sp)
        @test mat isa NakedPoints
        @test count(mat) == 2
    end

    @testset "translate" begin
        nk = NakedPoints([1.0, 2.0], (0.0, 5.0))
        nt = translate(nk, 10.0)
        @test point_values(nt) == [11.0, 12.0]
        @test bounds(nt) == (10.0, 15.0)
        # VariablePoints translate
        vp = VariablePoints(nk, [:a, :b])
        vt = translate(vp, 10.0)
        pv, mv = point_values(vt)
        @test pv == [11.0, 12.0]
        @test mv == [:a, :b]
        @test bounds(vt) == (10.0, 15.0)
    end

    @testset "points_intersects" begin
        nk1 = NakedPoints([1.0, 2.0], (0.0, 3.0))
        nk2 = NakedPoints([4.0, 5.0], (3.0, 6.0))
        nk3 = NakedPoints([1.5, 4.5], (0.0, 6.0))
        r1, r2 = points_intersects([nk1, nk2], [nk3])
        @test length(r1) == 2
        @test length(r2) == 2
    end

    @testset "pop_marks" begin
        # Create a VariablePoints with tuple marks (as pp_downsamp produces)
        nk = NakedPoints([1.0, 2.0], (0.0, 3.0))
        vp = VariablePoints(nk, [(1, :a), (2, :b)])
        popped = pop_marks(vp)
        _, marks = point_values(popped)
        @test collect(marks) == [:a, :b]
    end

    # --------------------------------------------------
    # 6. Downsampling
    # --------------------------------------------------
    @testset "pp_downsamp" begin
        pts = NakedPoints([0.1, 0.11, 0.12, 0.5, 0.51, 0.9], (0.0, 1.0))
        ds = pp_downsamp(pts, 0.0, 1.0, 0.1)
        # should merge close points
        @test count(ds) < 6
        # verify merge counts sum to original
        _, marks = point_values(ds)
        total = sum(m -> m[1], marks)
        @test total == 6
        # no merging needed (all far apart)
        pts_far = NakedPoints([0.1, 0.5, 0.9], (0.0, 1.0))
        ds_far = pp_downsamp(pts_far, 0.0, 1.0, 0.05)
        @test count(ds_far) == 3
        # all merge into one (resolution larger than range)
        pts_close = NakedPoints([0.1, 0.11, 0.12], (0.0, 1.0))
        ds_one = pp_downsamp(pts_close, 0.0, 1.0, 1.0)
        @test count(ds_one) == 1
    end

    @testset "pt_merge" begin
        # NakedPoints merge (averages positions)
        nps = [NakedPoint(1.0), NakedPoint(2.0), NakedPoint(3.0)]
        merged = pt_merge(nps)
        @test nakedpointvalue(merged) == 2.0
        # MarkedPoints merge (averages both)
        mps = [MarkedPoint(1.0, 10.0), MarkedPoint(3.0, 20.0)]
        merged_m = pt_merge(mps)
        @test nakedpointvalue(merged_m) == 2.0
        @test merged_m.mark == 15.0
    end

    @testset "pt_extent_merge" begin
        # MarkedPoints: extent + mean
        mps = [MarkedPoint(1.0, 10.0), MarkedPoint(3.0, 20.0), MarkedPoint(2.0, 30.0)]
        merged = pt_extent_merge(mps)
        @test nakedpointvalue(merged) == 2.0
        @test merged.mark[1] == (1.0, 3.0)  # extent
        @test merged.mark[2] == 20.0         # mean of marks
        # NakedPoints: extent
        nps = [NakedPoint(1.0), NakedPoint(4.0), NakedPoint(2.0)]
        merged_n = pt_extent_merge(nps)
        @test nakedpointvalue(merged_n) ≈ 7.0 / 3
        @test merged_n.mark == (1.0, 4.0)
    end

    # --------------------------------------------------
    # 7. Documentation examples
    # --------------------------------------------------

    @testset "index.md examples" begin
        # Example Workflow
        spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))

        trial = NakedInterval((2.0, 6.0))
        baseline = complement(EventIntervals.interval(spikes), trial)
        @test length(baseline) == 2
        @test bounds(baseline[1]) == (0.0, 2.0)
        @test bounds(baseline[2]) == (6.0, 10.0)

        trial_spikes = SubPoints(spikes, trial)
        @test rate(trial_spikes) == 0.75

        windows = chunk(NakedInterval((0.0, 10.0)), 2.5)
        @test length(windows) == 4
        @test bounds(windows[1]) == (0.0, 2.5)
        @test bounds(windows[4]) == (7.5, 10.0)

        window_rates = [rate(SubPoints(spikes, w)) for w in windows]
        @test window_rates == [1.2, 0.4, 0.4, 0.8]

        stim_intervals = [NakedInterval((1.0, 3.0)), NakedInterval((7.0, 9.0))]
        stim_spikes = interval_intersections_subpoints([spikes], stim_intervals)
        @test length(stim_spikes) == 2

        silence = shrink(complement(EventIntervals.interval(spikes), stim_intervals), 0.5)
        @test length(silence) == 3
        @test bounds(silence[1]) == (0.25, 0.75)

        downsampled = pp_downsamp(spikes, 0.0, 10.0, 1.0)
        @test count(downsampled) == 4
    end

    @testset "guide.md interval examples" begin
        # NakedInterval basics
        int = NakedInterval((0.0, 10.0))
        @test bounds(int) == (0.0, 10.0)
        @test measure(int) == 10.0
        @test midpoint(int) == 5.0

        # Two-arg constructor
        @test NakedInterval(0.0, 10.0) == int

        # MarkedInterval
        trial = MarkedInterval((0.0, 5.0), "stimulus_A")
        @test get_mark(trial) == "stimulus_A"
        @test bounds(trial) == (0.0, 5.0)
        @test measure(trial) == 5.0

        # RelativeInterval
        reference = NakedInterval((5.0, 8.0))
        rel = RelativeInterval(reference, true, NakedInterval((-1.0, 3.0)))
        @test bounds(rel) == (4.0, 8.0)

        # IntervalSet
        iset = IntervalSet(NakedInterval((0.0, 5.0)), MarkedInterval((5.0, 10.0), :stim))
        @test bounds(iset) == (0.0, 10.0)
        @test measure(iset) == 10.0
        @test get_mark(iset) == :stim

        # Complement (single)
        domain = NakedInterval((0.0, 10.0))
        stim = NakedInterval((3.0, 7.0))
        baseline = complement(domain, stim)
        @test length(baseline) == 2
        @test bounds(baseline[1]) == (0.0, 3.0)
        @test bounds(baseline[2]) == (7.0, 10.0)

        # Complement (vector)
        stims = [NakedInterval((2.0, 4.0)), NakedInterval((6.0, 8.0))]
        gaps = complement(domain, stims)
        @test length(gaps) == 3
        @test bounds(gaps[1]) == (0.0, 2.0)
        @test bounds(gaps[2]) == (4.0, 6.0)
        @test bounds(gaps[3]) == (8.0, 10.0)

        # Chunking
        chunks = chunk(NakedInterval((0.0, 10.0)), 3.0)
        @test length(chunks) == 4
        @test bounds(chunks[1]) == (0.0, 3.0)
        @test bounds(chunks[4]) == (9.0, 10.0)

        # Chunking exact
        chunks_exact = chunk(NakedInterval((0.0, 10.0)), 3.0, true)
        @test length(chunks_exact) == 3
        @test bounds(chunks_exact[3]) == (6.0, 9.0)

        # Chunking vector
        vchunks = chunk([NakedInterval((0.0, 5.0)), NakedInterval((10.0, 15.0))], 2.5)
        @test length(vchunks) == 4

        # Shrinking
        @test shrink(NakedInterval((0.0, 10.0)), 2.0) == NakedInterval((1.0, 9.0))

        # Shifting
        @test shift_interval(NakedInterval((0.0, 5.0)), 10.0) == NakedInterval((10.0, 15.0))

        # Shifting (curried)
        shifter = shift_interval(10.0)
        @test shifter(NakedInterval((0.0, 5.0))) == NakedInterval((10.0, 15.0))

        # Overlap depth
        intervals = [NakedInterval((0.0, 5.0)),
                     NakedInterval((3.0, 8.0)),
                     NakedInterval((6.0, 10.0))]
        levels = interval_levels(intervals)
        @test length(levels) == 5
        @test get_mark(levels[1]) == 1
        @test get_mark(levels[2]) == 2

        # Intersection
        @test interval_intersect(NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0))) ==
              NakedInterval((3.0, 5.0))

        # Pairwise intersections
        isects = interval_intersections(
            [NakedInterval((0.0, 5.0)), NakedInterval((7.0, 10.0))],
            [NakedInterval((3.0, 8.0))],
        )
        @test length(isects) == 2
        @test bounds(isects[1]) == (3.0, 5.0)
        @test bounds(isects[2]) == (7.0, 8.0)

        # Subinterval
        @test subinterval(NakedInterval((0.0, 10.0)), NakedInterval((2.0, 8.0))) ==
              NakedInterval((2.0, 8.0))

        # Relative coordinates
        rel_int = relative_interval(NakedInterval((5.0, 8.0)), NakedInterval((4.0, 10.0)))
        @test bounds(rel_int) == (1.0, 4.0)

        # Other utilities
        @test check_overlap(NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0))) == true
        @test is_subinterval(NakedInterval((2.0, 4.0)), NakedInterval((0.0, 10.0))) == true
        @test (5.0 in NakedInterval((0.0, 10.0))) == true
    end

    @testset "guide.md points examples" begin
        # NakedPoints construction
        spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))

        # Unsorted input
        spikes_unsorted = NakedPoints([9.1, 0.5, 4.7, 2.1, 1.3, 8.0, 5.2], NakedInterval((0.0, 10.0)))
        @test point_values(spikes_unsorted) == point_values(spikes)

        # Tuple and two-arg constructors
        np_tuple = NakedPoints([1.0, 2.0, 3.0], (0.0, 5.0))
        np_twoarg = NakedPoints([1.0, 2.0, 3.0], 0.0, 5.0)
        @test point_values(np_tuple) == point_values(np_twoarg)

        # Inferred interval
        np_infer = NakedPoints([1.0, 2.0, 3.0])
        @test bounds(np_infer) == (1.0, 3.0)

        # Basic queries
        @test count(spikes) == 7
        @test duration(spikes) == 10.0
        @test rate(spikes) == 0.7
        @test bounds(spikes) == (0.0, 10.0)
        @test EventIntervals.interval(spikes) == NakedInterval((0.0, 10.0))
        @test count(spikes, 2.0, 6.0) == 3
        @test rate(spikes, 2.0, 6.0) == 0.75

        # Translation
        shifted = translate(spikes, 100.0)
        @test bounds(shifted) == (100.0, 110.0)

        # VariablePoints
        np = NakedPoints([1.0, 2.0, 3.0], NakedInterval((0.0, 5.0)))
        vp = VariablePoints(np, [:a, :b, :c])
        @test vp[1] == MarkedPoint(1.0, :a)

        # SubPoints
        trial = SubPoints(spikes, NakedInterval((2.0, 6.0)))
        @test count(trial) == 3
        @test rate(trial) == 0.75
        @test duration(trial) == 4.0

        # SubPoints alternative constructors
        @test count(SubPoints(spikes, (2.0, 6.0))) == 3
        @test count(SubPoints(spikes, 2.0, 6.0)) == 3

        # maybe_subpoints
        @test maybe_subpoints(spikes, NakedInterval((2.0, 6.0))) !== nothing
        @test maybe_subpoints(spikes, NakedInterval((20.0, 30.0))) === nothing

        # Mark operations
        p = NakedPoint(1.0)
        mp = push_mark(p, :neuron_A)
        @test mp == MarkedPoint(1.0, :neuron_A)
        mp2 = push_mark(mp, 42)
        @test mp2 == MarkedPoint(1.0, (42, :neuron_A))
        inner, outer_mark = pop_mark(mp2)
        @test inner == MarkedPoint(1.0, (:neuron_A,))
        @test outer_mark == 42

        # join_points
        a = NakedPoints([1.0, 3.0], NakedInterval((0.0, 5.0)))
        b = NakedPoints([2.0, 7.0], NakedInterval((0.0, 10.0)))
        joined = join_points(a, b)
        @test count(joined) == 4
        @test bounds(joined) == (0.0, 10.0)

        # points_intersects
        pts1 = [NakedPoints([1.0, 2.0], NakedInterval((0.0, 5.0)))]
        pts2 = [NakedPoints([3.0, 6.0], NakedInterval((2.0, 8.0)))]
        sub1, sub2 = points_intersects(pts1, pts2)
        @test length(sub1) == 1
        @test length(sub2) == 1

        # Downsampling
        dense = NakedPoints([1.0, 1.1, 1.2, 5.0, 5.05, 9.0], NakedInterval((0.0, 10.0)))
        ds = pp_downsamp(dense, 0.0, 10.0, 0.5)
        @test count(ds) < 6

        # Aggregate rate
        t1 = SubPoints(spikes, NakedInterval((0.0, 5.0)))
        t2 = SubPoints(spikes, NakedInterval((5.0, 10.0)))
        @test rate([t1, t2]) == rate(spikes)
    end
end
