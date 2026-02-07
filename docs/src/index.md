```@meta
CurrentModule = EventIntervals
```

# EventIntervals.jl

EventIntervals provides interval-aware data structures for point processes. It couples timestamped event collections with the temporal domains they are defined on, so that slicing, intersecting, downsampling, and transforming operations keep points and their intervals in sync.

## Motivation

Working with recorded event data — spike trains, calcium-imaging transients, behavioral syllable onsets — requires two abstractions that are tightly coupled in practice but rarely coupled in code: **collections of timestamped events** and **the temporal intervals they live on**. A spike train is not just a sorted vector of times; it is a sorted vector of times *defined on a recording interval*, and operations on it (computing a firing rate, windowing to a trial, intersecting with another recording) must respect that domain.

EventIntervals makes this coupling explicit. Every `Points` object carries its domain interval, and the package provides operations that act on both together: windowing produces a lazy `SubPoints` view rather than a copy, rate calculations use the interval's measure as the denominator, and joining two point processes produces a result whose interval is the union of the inputs.

The interval layer is equally first-class. Intervals can carry metadata (`MarkedInterval`), be expressed in relative coordinates (`RelativeInterval`), or be grouped into contiguous segments (`IntervalSet`). Operations like complement, chunking, overlap-depth calculation, and shrinking work on these types directly.

## Design Principles

**Points live on intervals.** Every `Points` collection has an associated `Interval` that defines its temporal domain. Constructors validate that all points fall within their interval, and operations propagate intervals automatically.

**Naked and marked variants.** Each core type comes in two forms: a lightweight "naked" version carrying only coordinates, and a "marked" version carrying arbitrary metadata. `NakedPoint` vs. `MarkedPoint`, `NakedInterval` vs. `MarkedInterval`, `NakedPoints` vs. `VariablePoints`. This means zero overhead when you don't need metadata, with a smooth upgrade path when you do.

**Lazy views over copies.** `SubPoints` provides a zero-copy view of a point collection within a sub-interval, backed by binary search for efficient range queries. This makes repeated windowing operations (e.g., slicing a long recording into hundreds of trial windows) cheap.

**Immutable collections.** `Points` types implement `AbstractVector` but are read-only. This ensures data integrity when multiple `SubPoints` views reference the same underlying data.

**Composable marks.** Marks on points can be stacked using tuple nesting via `push_mark` and `pop_mark`, allowing metadata to accumulate through processing pipelines (e.g., downsampling pushes a merge count onto each point's mark stack).

## Type Hierarchy

```
Point{E}
├── NakedPoint{E}              # bare timestamp
└── MarkedPoint{E,M}           # timestamp + metadata

Interval{E,N}
├── NakedInterval{D}           # (start, stop) bounds
├── MarkedInterval{D,M}        # interval + metadata
├── RelativeInterval{D,I,J}    # interval in relative coordinates
└── IntervalSet{E,T}           # contiguous interval segments

Points{E,N,I,T} <: AbstractVector{T}
├── NakedPoints{E,I,A}         # sorted timestamps on an interval
├── VariablePoints{E,I,P,M,A}  # sorted timestamps + per-point marks
└── SubPoints{E,M,I,P}         # lazy view into another Points
```

## Example Workflow

A typical workflow for trial-based neural data analysis:

```julia
using EventIntervals

# Spike times from a 10-second recording
spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))

# Define trial and inter-trial intervals
trial = NakedInterval((2.0, 6.0))
baseline = complement(interval(spikes), trial)

# View spikes within the trial — no data copied
trial_spikes = SubPoints(spikes, trial)
rate(trial_spikes)  # firing rate within the trial window

# Break a long interval into fixed-size analysis windows
windows = chunk(NakedInterval((0.0, 10.0)), 2.5)
# 4-element Vector{NakedInterval{Float64}}:
#   (0.0, 2.5), (2.5, 5.0), (5.0, 7.5), (7.5, 10.0)

# Windowed firing rate: slice spikes into each window
window_rates = [rate(SubPoints(spikes, w)) for w in windows]

# Restrict spikes to only the windows that overlap a set of stimulus intervals
stim_intervals = [NakedInterval((1.0, 3.0)), NakedInterval((7.0, 9.0))]
stim_spikes = interval_intersections_subpoints([spikes], stim_intervals)

# Find silence periods: complement the stimulus intervals, then shrink edges
silence = shrink(complement(interval(spikes), stim_intervals), 0.5)

# Downsample dense spike trains for visualization
downsampled = pp_downsamp(spikes, 0.0, 10.0, 1.0)
```

## Ecosystem Context

EventIntervals occupies a specific niche in the Julia ecosystem. It is a **data manipulation** package for observed point process realizations, not a statistical modeling or simulation framework.

| Package | Best for |
|---|---|
| **EventIntervals.jl** | Manipulating observed event data on temporal intervals |
| [SortedIntervals.jl](https://github.com/galenlynch/SortedIntervals.jl) | Batch set operations on sorted interval lists (used internally) |
| [IntervalSets.jl](https://github.com/JuliaMath/IntervalSets.jl) | Type-safe single-interval representations |
| [IntervalTrees.jl](https://github.com/BioJulia/IntervalTrees.jl) | Repeated queries on large static interval collections |
| [JumpProcesses.jl](https://github.com/SciML/JumpProcesses.jl) | Simulating and fitting point process models |

Packages like JumpProcesses.jl and PointProcesses.jl define generative models and fit parameters; EventIntervals stores, slices, and transforms the data those models produce or that experiments record. They are complementary: you might simulate with JumpProcesses and analyze the output with EventIntervals, or sort spikes with SpikeSorting.jl and then manage the resulting spike trains with EventIntervals.

See the [Usage Guide](@ref guide) for detailed examples, or the [API Reference](@ref api) for complete function signatures.
