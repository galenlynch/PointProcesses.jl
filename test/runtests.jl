using PointProcesses
using Random
using Test

const srand = Random.seed!

@testset "PointProcesses" begin
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
end
