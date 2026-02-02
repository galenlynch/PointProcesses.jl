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
end
