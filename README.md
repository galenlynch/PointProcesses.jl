# EventIntervals [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://galenlynch.github.io/EventIntervals.jl/stable/) [![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://galenlynch.github.io/EventIntervals.jl/dev/) [![Build Status](https://github.com/galenlynch/EventIntervals.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/galenlynch/EventIntervals.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Coverage](https://codecov.io/gh/galenlynch/EventIntervals.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/galenlynch/EventIntervals.jl)

Interval-aware data structures for point processes. Store timestamped events on defined temporal domains, then slice, intersect, downsample, and transform them with operations that keep points and their intervals in sync.

## Quick Example

```julia
using EventIntervals

# Spike times from a recording session
spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))

# Count and rate over the full session
count(spikes)       # 7
rate(spikes)        # 0.7

# View spikes in a sub-interval — no copy, just a lazy window
trial = SubPoints(spikes, NakedInterval((2.0, 6.0)))
count(trial)        # 3
rate(trial)         # 0.75

# Attach per-spike metadata
marked = VariablePoints(spikes, ["fast", "fast", "slow", "fast", "slow", "slow", "fast"])
```

## When to Use What

| Package | Best for |
|---|---|
| **EventIntervals.jl** | Manipulating observed event data on temporal intervals |
| [SortedIntervals.jl](https://github.com/galenlynch/SortedIntervals.jl) | Batch set operations on sorted interval lists |
| [IntervalSets.jl](https://github.com/JuliaMath/IntervalSets.jl) | Type-safe single-interval representations |
| [IntervalTrees.jl](https://github.com/BioJulia/IntervalTrees.jl) | Repeated queries on large static interval collections |
| [JumpProcesses.jl](https://github.com/SciML/JumpProcesses.jl) | Simulating and fitting point process models |

## Installation

```julia
using Pkg
Pkg.add(url="https://github.com/galenlynch/EventIntervals.jl")
```

## Documentation

See the [full documentation](https://galenlynch.github.io/EventIntervals.jl/dev/) for a usage guide and API reference.

## Citing

See [`CITATION.bib`](CITATION.bib) for the relevant reference(s).
