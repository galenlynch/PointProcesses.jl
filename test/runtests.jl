using PointProcesses
@static if VERSION < v"0.7.0-DEV.2005"
    using Base.Test
else
    using Test
end

@testset "PointProcesses" begin
    pp_testarr = [1, 2, 3]
    pp_notshuff_arr = [2, 1, 3]
    interval = (0, 5)
    NakedPointProcess(pp_notshuff_arr)
    NakedPointProcess(pp_testarr, interval[1], interval[2])
    NakedPointProcess(pp_testarr)
    spp = NakedPointProcess(pp_testarr, interval)
    @test duration(spp) == 5
    @test count(spp) == 3
    @test count(spp, 2, 5) == 2
    @test point_values(spp) == [1, 2, 3]
    @test all(point_values(spp, 0, 1) .== [1])

    mag = [2, 4, 6]
    VariablePointProcess(spp, mag)
    mpp = VariablePointProcess(pp_testarr, mag, interval)
    @test duration(mpp) == 5
    @test count(mpp) == 3
    @test count(mpp, 2, 5) == 2
    @test point_values(mpp) == (pp_testarr, mag)
    p, m = point_values(mpp, 2, 8)
    @test all(p .== [2, 3])
    @test all(m .== [4, 6])

    spp = SubPointProcess(spp, (2, 5))
    spp = SubPointProcess(spp, 2, 5)
    @test duration(spp) == 3
    @test count(spp) == 2
    @test count(spp, 1, 5) == 2
    @test all(point_values(spp, 1, 5) .== [2, 3])

    srand(1)
    big_A = rand(20)
    pp_big = NakedPointProcess(big_A, 0, 1)
    pp_downsamp(pp_big, 0, 1, 0.05)
end
