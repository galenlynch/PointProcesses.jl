using EventIntervals
using Documenter

DocMeta.setdocmeta!(EventIntervals, :DocTestSetup, :(using EventIntervals); recursive=true)

makedocs(;
    modules=[EventIntervals],
    authors="Galen Lynch <galen@galenlynch.com>",
    sitename="EventIntervals.jl",
    format=Documenter.HTML(;
        canonical="https://galenlynch.github.io/EventIntervals.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/galenlynch/EventIntervals.jl",
    devbranch="main",
)
