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
    warnonly=[:missing_docs],
    pages=[
        "Home" => "index.md",
        "Usage Guide" => "guide.md",
        "API Reference" => "api.md",
    ],
)

deploydocs(;
    repo="github.com/galenlynch/EventIntervals.jl",
    devbranch="main",
)
