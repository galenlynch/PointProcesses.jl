```@meta
CurrentModule = EventIntervals
```

# [Usage Guide](@id guide)

## Intervals

Intervals represent bounded temporal domains. The simplest form is a [`NakedInterval`](@ref), which wraps a `(start, stop)` tuple:

```julia
int = NakedInterval((0.0, 10.0))
bounds(int)    # (0.0, 10.0)
measure(int)   # 10.0
midpoint(int)  # 5.0
```

You can also construct one from two arguments:

```julia
NakedInterval(0.0, 10.0)
```

### Marked Intervals

[`MarkedInterval`](@ref) attaches arbitrary metadata to an interval:

```julia
trial = MarkedInterval((0.0, 5.0), "stimulus_A")
get_mark(trial)   # "stimulus_A"
bounds(trial)      # (0.0, 5.0)
measure(trial)     # 5.0
```

Marks can be any type — strings, numbers, symbols, or custom structs.

### Relative Intervals

[`RelativeInterval`](@ref) expresses an interval in coordinates relative to a reference interval. This is useful for defining peri-event windows (e.g., "100 ms before to 300 ms after stimulus onset"):

```julia
reference = NakedInterval((5.0, 8.0))
rel = RelativeInterval(reference, true, NakedInterval((-1.0, 3.0)))
bounds(rel)  # (4.0, 8.0) — anchored to the left edge of reference
```

The `anchored_left` flag controls whether offsets are relative to the start or end of the reference.

### Interval Sets

[`IntervalSet`](@ref) groups contiguous intervals into a single object. The intervals must touch (no gaps) and be ordered:

```julia
iset = IntervalSet(NakedInterval((0.0, 5.0)), MarkedInterval((5.0, 10.0), :stim))
bounds(iset)    # (0.0, 10.0)
measure(iset)   # 10.0
get_mark(iset)  # :stim — returns the mark of the first marked interval
```

### Interval Operations

**Complement** — find the portions of a domain not covered by an interval or set of intervals:

```julia
domain = NakedInterval((0.0, 10.0))
stim = NakedInterval((3.0, 7.0))
baseline = complement(domain, stim)
# [NakedInterval((0.0, 3.0)), NakedInterval((7.0, 10.0))]
```

`complement` also accepts a vector of intervals:

```julia
stims = [NakedInterval((2.0, 4.0)), NakedInterval((6.0, 8.0))]
gaps = complement(domain, stims)
# [NakedInterval((0.0, 2.0)), NakedInterval((4.0, 6.0)), NakedInterval((8.0, 10.0))]
```

**Chunking** — break an interval into fixed-size pieces:

```julia
chunks = chunk(NakedInterval((0.0, 10.0)), 3.0)
# [NakedInterval((0.0, 3.0)), NakedInterval((3.0, 6.0)),
#  NakedInterval((6.0, 9.0)), NakedInterval((9.0, 10.0))]
```

Pass `exact=true` to drop any remainder that is shorter than `chunk_len`:

```julia
chunks = chunk(NakedInterval((0.0, 10.0)), 3.0, true)
# [NakedInterval((0.0, 3.0)), NakedInterval((3.0, 6.0)), NakedInterval((6.0, 9.0))]
```

Chunking also works on a vector of intervals, concatenating the results:

```julia
chunk([NakedInterval((0.0, 5.0)), NakedInterval((10.0, 15.0))], 2.5)
```

**Shrinking** — contract an interval inward from both ends:

```julia
shrink(NakedInterval((0.0, 10.0)), 2.0)
# NakedInterval((1.0, 9.0))
```

When applied to a vector, intervals too short to survive the shrink are dropped.

**Shifting** — translate an interval by an offset:

```julia
shift_interval(NakedInterval((0.0, 5.0)), 10.0)
# NakedInterval((10.0, 15.0))
```

`shift_interval` preserves marks on `MarkedInterval` and also works as a curried function:

```julia
shifter = shift_interval(10.0)
shifter(NakedInterval((0.0, 5.0)))  # NakedInterval((10.0, 15.0))
```

**Overlap depth** — compute how many intervals overlap at each position:

```julia
intervals = [NakedInterval((0.0, 5.0)),
             NakedInterval((3.0, 8.0)),
             NakedInterval((6.0, 10.0))]
levels = interval_levels(intervals)
```

[`interval_levels`](@ref) returns a vector of `MarkedInterval{Float64,Int}` where the mark is the overlap count. Assumes input is sorted by start time.

**Intersection and subintervals** — find overlapping regions:

```julia
interval_intersect(NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0)))
# NakedInterval((3.0, 5.0))

# Pairwise intersections between two sorted, non-overlapping vectors
interval_intersections([NakedInterval((0.0, 5.0)), NakedInterval((7.0, 10.0))],
                       [NakedInterval((3.0, 8.0))])
# [NakedInterval((3.0, 5.0)), NakedInterval((7.0, 8.0))]
```

[`subinterval`](@ref) validates that one interval is contained in another:

```julia
subinterval(NakedInterval((0.0, 10.0)), NakedInterval((2.0, 8.0)))
# NakedInterval((2.0, 8.0)) — validated as a subinterval
```

**Relative coordinates** — express an interval relative to a reference:

```julia
relative_interval(NakedInterval((5.0, 8.0)), NakedInterval((4.0, 10.0)))
```

**Other utilities:**

```julia
check_overlap(NakedInterval((0.0, 5.0)), NakedInterval((3.0, 8.0)))  # true
is_subinterval(NakedInterval((2.0, 4.0)), NakedInterval((0.0, 10.0)))  # true
5.0 in NakedInterval((0.0, 10.0))  # true
```

## Points

### NakedPoints

[`NakedPoints`](@ref) stores a sorted vector of timestamps on a defined interval:

```julia
spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))
```

The constructor validates that points are sorted and fall within the interval. If not sorted, it will sort them:

```julia
# Unsorted input — automatically sorted
spikes = NakedPoints([9.1, 0.5, 4.7, 2.1, 1.3, 8.0, 5.2], NakedInterval((0.0, 10.0)))
```

You can also construct from a tuple or two numbers for the interval bounds:

```julia
NakedPoints([1.0, 2.0, 3.0], (0.0, 5.0))
NakedPoints([1.0, 2.0, 3.0], 0.0, 5.0)
```

Or let the interval be inferred from the data:

```julia
NakedPoints([1.0, 2.0, 3.0])  # interval = (1.0, 3.0)
```

**Basic queries:**

```julia
count(spikes)           # 7
duration(spikes)        # 10.0
rate(spikes)            # 0.7 (count / duration)
bounds(spikes)          # (0.0, 10.0)
interval(spikes)        # NakedInterval((0.0, 10.0))

# Count and rate within a sub-range
count(spikes, 2.0, 6.0)   # 3
rate(spikes, 2.0, 6.0)    # 0.75
```

**Extracting values:**

```julia
point_values(spikes)           # the underlying vector
point_values(spikes, 2.0, 6.0) # view of values in [2.0, 6.0]
nakedvalues(spikes)            # same as point_values for NakedPoints
```

**Translation** — shift all points and their interval by a constant offset:

```julia
shifted = translate(spikes, 100.0)
bounds(shifted)  # (100.0, 110.0)
```

### VariablePoints

[`VariablePoints`](@ref) pairs each timestamp with a mark (metadata value). Construct from a `NakedPoints` and a marks vector:

```julia
np = NakedPoints([1.0, 2.0, 3.0], NakedInterval((0.0, 5.0)))
vp = VariablePoints(np, [:a, :b, :c])
```

Individual elements are `MarkedPoint` objects:

```julia
vp[1]  # MarkedPoint(1.0, :a)
```

**Extracting values and marks:**

```julia
point_values(vp)            # (times_vector, marks_vector)
point_values(vp, 1.0, 2.5)  # (view of times, view of marks) in range
get_mark(vp)                 # the full marks vector
```

All `Points` operations (`count`, `rate`, `duration`, `bounds`, `translate`) work on `VariablePoints`.

### SubPoints

[`SubPoints`](@ref) is a lazy, zero-copy view of a `Points` collection restricted to a sub-interval:

```julia
spikes = NakedPoints([0.5, 1.3, 2.1, 4.7, 5.2, 8.0, 9.1], NakedInterval((0.0, 10.0)))
trial = SubPoints(spikes, NakedInterval((2.0, 6.0)))

count(trial)     # 3 — only points in [2.0, 6.0]
rate(trial)      # 0.75
duration(trial)  # 4.0
```

The constructor validates that the sub-interval is contained within the parent:

```julia
SubPoints(spikes, NakedInterval((2.0, 6.0)))  # OK
SubPoints(spikes, (2.0, 6.0))                  # tuple form also works
SubPoints(spikes, 2.0, 6.0)                    # two-argument form
```

Nesting `SubPoints` flattens automatically — the inner view references the original data, not the intermediate view.

[`maybe_subpoints`](@ref) returns a `SubPoints` if the interval intersects, or `nothing` if it doesn't:

```julia
maybe_subpoints(spikes, NakedInterval((2.0, 6.0)))   # SubPoints(...)
maybe_subpoints(spikes, NakedInterval((20.0, 30.0)))  # nothing
```

### Mark Operations

Marks can be stacked and unstacked using tuple nesting:

```julia
p = NakedPoint(1.0)
mp = push_mark(p, :neuron_A)      # MarkedPoint(1.0, :neuron_A)
mp2 = push_mark(mp, 42)           # MarkedPoint(1.0, (42, :neuron_A))
inner, outer_mark = pop_mark(mp2)  # (MarkedPoint(1.0, (:neuron_A,)), 42)
```

[`pop_marks`](@ref) strips the outermost mark from all points in a `VariablePoints`.

### Joining Points

[`join_points`](@ref) merges multiple point collections. The result's interval is the union of the inputs:

```julia
a = NakedPoints([1.0, 3.0], NakedInterval((0.0, 5.0)))
b = NakedPoints([2.0, 7.0], NakedInterval((0.0, 10.0)))
joined = join_points(a, b)
# 4 points on interval (0.0, 10.0), sorted
```

Works for both naked and marked points, and accepts any number of arguments:

```julia
join_points(a, b, c)  # merges three collections
```

### Intersecting Point Collections

[`points_intersects`](@ref) finds the intersection of two vectors of `Points` — restricting each collection to only the time ranges where both have coverage:

```julia
pts1 = [NakedPoints([1.0, 2.0], NakedInterval((0.0, 5.0)))]
pts2 = [NakedPoints([3.0, 6.0], NakedInterval((2.0, 8.0)))]
sub1, sub2 = points_intersects(pts1, pts2)
```

[`interval_intersections_subpoints`](@ref) is the lower-level function that intersects a vector of `Points` with a vector of `Interval` objects, returning `SubPoints` views.

### Downsampling

[`pp_downsamp`](@ref) merges points that are closer together than a given resolution, useful for reducing dense event data for visualization:

```julia
spikes = NakedPoints([1.0, 1.1, 1.2, 5.0, 5.05, 9.0], NakedInterval((0.0, 10.0)))
ds = pp_downsamp(spikes, 0.0, 10.0, 0.5)
```

Points within `resolution` of each other are merged using a merge function. The default [`pt_merge`](@ref) averages timestamps (and marks, if present). [`pt_extent_merge`](@ref) preserves the extent (min, max) of merged points as an additional mark.

Each merged point gets a count pushed onto its mark stack, recording how many original points were combined.

### Aggregate Rate

`rate` can also be computed over a vector of `Points`:

```julia
rate([trial_spikes_1, trial_spikes_2, trial_spikes_3])
# total spike count across all trials / total duration
```
