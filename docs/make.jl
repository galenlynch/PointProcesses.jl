using PointProcesses
using Documenter

DocMeta.setdocmeta!(PointProcesses, :DocTestSetup, :(using PointProcesses); recursive=true)

makedocs(;
    modules=[PointProcesses],
    authors="Galen Lynch <galen@galenlynch.com>",
    sitename="PointProcesses.jl",
    format=Documenter.HTML(;
        canonical="https://galenlynch.github.io/PointProcesses.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/galenlynch/PointProcesses.jl",
    devbranch="main",
)
