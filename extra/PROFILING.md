# Haxe Compiler Profiling Guide & Analysis

## How to Build a Profiling-Enabled Binary

The `src/dune` file includes a `profile` build environment that adds DWARF
debug info (`-g`) and keeps optimisations (`-O2`), so `perf` can resolve
OCaml symbols without a significant performance penalty:

```bash
eval $(opam env)
dune build --profile profile src/haxe.exe
# binary is _build/default/src/haxe.exe
```

## How to Profile

### Built-in Timer Breakdown (recommended first step)

```bash
# Overall phase timing
haxe --cwd tests/unit compile-macro.hxml --times

# Detailed per-method eval timing
haxe --cwd tests/unit compile-macro.hxml --times -D times.eval

# Detailed filter + analyzer timing
haxe --cwd tests/unit compile-macro.hxml --times \
  -D times.filter=2 -D times.analyzer=2

# HXB-specific timing
haxe --cwd tests/unit compile-hxb-interp-roundtrip.hxml --times -D times.hxb
```

### `perf` (Linux, requires root or `perf_event_paranoid <= 1`)

```bash
sudo sysctl -w kernel.perf_event_paranoid=-1

# Record with DWARF call-graph unwinding
perf record -F 999 -g --call-graph dwarf -o eval.perf \
  haxe --cwd tests/unit compile-macro.hxml

# Flat profile (top self-time functions)
perf report -i eval.perf --no-children --stdio --call-graph=none

# Callers of a specific symbol
perf report -i eval.perf --children --stdio \
  --call-graph=caller --symbol-filter=compare_val
```

### `olly` (OCaml 5 runtime-events GC profiler)

```bash
opam install runtime_events_tools
olly gc-stats -- haxe --cwd tests/unit compile-macro.hxml
```

### `memtrace` (allocation profiling)

**Note:** `memtrace` does **not** work with OCaml 5 multicore (`Gc.Memprof`
is disabled). Use `olly gc-stats` for GC overhead metrics, or build a
single-domain binary to use `memtrace`.

---

## Profiling Results — Eval Unit Tests (~2.7 s)

### `--times` Phase Breakdown

| Phase | Time (s) | % |
|-------|----------|---|
| Macro execution | 1.06 | 36 |
| — `ancestorHasInitializeUtest` (utest build macro) | 0.23 | 8 |
| Typing | 0.76 | 26 |
| Parsing | 0.33 | 11 |
| Filters | 0.31 | 11 |
| — `handle_abstract_casts` | 0.05 | 2 |
| — `fix_return_dynamic_from_void_function` | 0.06 | 2 |
| Analyzer | 0.24 | 8 |
| — fusion / fuse | 0.07 | 2 |
| Interp (eval JIT) | 0.16 | 6 |

### `perf` Flat Profile (Top Self-Time Functions)

| % | Symbol | Category |
|---|--------|----------|
| 13.5 | `do_some_marking` | **GC major marking** |
| 5.3 | `caml_shared_try_alloc` | GC allocation |
| 3.7 | `oldify_one` | GC minor→major promotion |
| 3.1 | `compare_val` | **Polymorphic comparison** |
| 2.6 | `pool_sweep` | GC sweep |
| 2.4 | `Texpr.map_expr` | Type expression traversal |
| 2.4 | `caml_hash` | Hash-table operations |
| 2.4 | `oldify_mopup` | GC |
| 1.0 | `TFunctions.follow` | Type follow |
| 0.96 | `caml_alloc_string` | String allocation |
| 0.84 | `Stdlib.List.map` | List processing |
| 0.70 | `Dce.expr` | Dead-code elimination |
| 0.62 | `Stdlib.Map.find` | Map lookup |
| 0.45 | `EvalJit.loop` | JIT compilation |

### `olly` GC Statistics

| Metric | Value |
|--------|-------|
| Wall time | 2.98 s |
| CPU time | 5.27 s |
| GC time | 1.38 s |
| **GC overhead (% of CPU)** | **26.3 %** |
| Domain 0 GC overhead | 30.7 % |
| Worker domains (1-3) GC | 19-22 % |
| P99 GC latency | 3.0 ms |
| Max GC latency | 6.0 ms |

---

## Profiling Results — HXB Roundtrip

### Write Phase (+0.38 s over normal eval)

The HXB writing adds `generate/hxb` at 12-13 % of total time. `perf` shows
`HxbWriter.loop` (0.61 %) and `HxbWriter.write_type_instance` (0.38 %) as
the top writer functions. `Zlib.update_crc` (1.19 %) appears due to
zip compression.

**After switching to `Stored` (level 0) compression**, `Zlib.update_crc`
disappears from the profile entirely.

### Read Phase (~0.85 s — 3× faster than full compile)

| Phase | Time (s) | % |
|-------|----------|---|
| Typing (HXB deserialization) | 0.33 | 39 |
| Interp (eval JIT) | 0.30 | 35 |
| Filters | 0.11 | 13 |
| hxblib I/O | 0.03 | 4 |
| — `get bytes` (zip read) | 0.02 | 72 % of hxblib |

With `Stored` compression, `hxblib get bytes` dropped from **0.090 s → 0.021 s**
(4.3× faster). Archive size went from 3.6 MB → 6.5 MB (1.8× larger).

### Roundtrip `olly` GC Statistics

| Metric | Value |
|--------|-------|
| Wall time | 3.49 s |
| GC time | 1.61 s |
| **GC overhead** | **43.5 %** |
| Domain 0 GC overhead | 32.1 % |

The higher GC overhead in roundtrip comes from the HXB write phase creating
many temporary serialization buffers.

---

## Identified Hotspots & Recommendations

### 1. GC Pressure (~26 % of CPU)

The OCaml 5 GC (with multicore support) shows significant overhead.
`do_some_marking` alone accounts for 13.5 % of self-time. While OCaml's
generational GC handles short-lived allocations well, the sheer volume of
allocations in type traversal (`Texpr.map_expr`, `TFunctions.follow`,
`Stdlib.List.map`) creates GC pressure.

**Note:** `memtrace` is incompatible with OCaml 5 multicore. To profile
allocations at the source level, either:
- Build a single-domain (non-multicore) OCaml switch and use `memtrace`, or
- Use OCaml 5's `runtime_events` with `olly gc-stats` for aggregate GC
  metrics (already done above).

### 2. Polymorphic Comparison (`compare_val`, 3.1 %)

`perf` shows `compare_val` consumes 3.1 % of total time, called via both
`caml_compare` (2 %) and `caml_equal` (1 %). These are triggered by OCaml's
polymorphic `(=)` and `compare` operators.

**Important:** OCaml's native compiler specialises `=` when the types
are known at compile-time, but **only for types whose constructors are all
constant** (take no arguments). If any constructor of the type carries
data (e.g. `Var of var_kind`), OCaml emits a call to `caml_equal` even
when the concrete values at runtime might be constant constructors.

One exception: when one side of `=` is a **literal constant constructor**
(e.g. `x = Const`), OCaml recognises that the constant constructor is
an immediate and emits a direct `cmpq` regardless of whether the type
has structured variants.

Confirmed by inspecting the generated assembly (`ocamlfind ocamlopt -S`):

| Expression | Type | Assembly | Polymorphic? |
|---|---|---|---|
| `(a : method_kind) = (b : method_kind)` | all-constant ctors | `cmpq` | No |
| `(m : method_kind) = MethDynamic` | literal constant | `cmpq` | No |
| `!(a.a_status) = Const` | literal constant | `cmpq` | No |
| `mkind m1 = mkind m2` | `int = int` | `cmpq` | No |
| `(a : field_kind) = (b : field_kind)` | has `Var of var_kind` | `caml_equal` | **Yes** |
| `(a : tconstant) = (b : tconstant)` | has `TInt of int32` etc. | `caml_equal` | **Yes** |
| `e.eexpr = TConst TSuper` | literal structured ctor | `caml_equal` | **Yes** (but shallow) |
| `(a : var_access) = (b : var_access)` | has `AccRequire of ...` | `caml_equal` | **Yes** |
| `(a : path) = (b : path)` | `string list * string` | `caml_equal` | **Yes** |

**Confirmed polymorphic call sites:**

1. **`src/typing/typeloadCheck.ml:187`** — `| a, b when a = b -> ()`:
   compares two `field_kind` variables. `field_kind` has `Var of var_kind`,
   so OCaml cannot specialise this. Fixing: decompose into a pattern match
   or a custom `field_kind_eq` helper.
2. **`src/typing/fields.ml:141,194`** — `e.eexpr = TConst TSuper`:
   `texpr_expr` is massively structured. However, this comparison is
   **shallow** — `caml_equal` checks the constructor tag first, and both
   `TConst` and `TSuper` are quickly resolved. Low priority.
3. **`src/optimization/optimizerTexpr.ml:189`** — `a = b` comparing two
   `tconstant` values (has `TInt of int32`, `TString of string`, etc.).
4. **Path comparisons** (~10 sites) — `c.cl_path = path` compares
   `string list * string` tuples.
5. **`src/typing/nullSafety.ml`** — polymorphic `Hashtbl` with
   `safety_subject` keys (a variant with `SFieldOfClass of path * string list`
   etc.). Every `Hashtbl.find`/`Hashtbl.mem`/`Hashtbl.replace` call
   triggers both `caml_hash` and `caml_equal`.

**Not polymorphic (previously incorrectly listed):**

- `typeloadCheck.ml:66` — `not (m1 = MethDynamic)`: `method_kind` has only
  constant constructors → direct `cmpq`.
- `typeloadCheck.ml:432` — `mkind m1 = mkind m2`: projects to `int` first
  → direct `cmpq`.
- `fields.ml:41` — `!(a.a_status) = Const`: comparing against a literal
  constant constructor → direct `cmpq`.

**Recommendation:** The total cost is modest (3.1 %). The most impactful
fix would be switching `nullSafety.ml` to functorized hash tables with a
custom hash/equal for `safety_subject`, which would also eliminate the
`caml_hash` overhead (2.4 % of perf time, much of which likely comes from
these tables). The `field_kind = field_kind` comparison at
`typeloadCheck.ml:187` can be replaced with a pattern match.

### 3. HXB Zip I/O

Changing from `Deflated` (level 6) to `Stored` (level 0) eliminates all
compression/decompression overhead. This was implemented in this PR.

- Read-phase `get bytes`: 0.090 s → 0.021 s (4.3× improvement)
- Archive size: 3.6 MB → 6.5 MB (1.8× increase)

### 4. HxbWriter Type Instance Handling

The writer already deduplicates type instances within expression contexts
via `write_texpr_type_instance` (serialises to bytes, then interns via
`StringPool`). Top-level field-signature type writes are not deduplicated
but account for far fewer calls. The `perf` data shows
`HxbWriter.write_type_instance` at only 0.38 % of total time, so further
deduplication here would yield diminishing returns.

### 5. Domain Management — Domainslib replaced with WorkerPool

**Problem:** Domainslib's `Task.setup_pool` spawns N OS-level domains that
spin-wait on a lock-free multi-channel. Even when there is no work to do,
each domain busy-loops, consuming CPU. For single-file compilations or
eval-only runs, the pool is acquired but domains sit idle for 80 %+ of the
compilation.

**Solution (implemented in this PR):** Replaced Domainslib entirely with a
custom `WorkerPool` in `parallel.ml`. The pool uses `Domain.spawn` for
workers that block on `Condition.wait` between calls — **zero CPU when
idle**, unlike Domainslib's spin-wait.

The `domainslib` dependency has been removed from `src/dune` and
`haxe.opam`.

**Architecture:**

```
                    ┌─── Worker 0: Condition.wait → process chunk → signal done ───┐
 submit(length, f) ─┼─── Worker 1: Condition.wait → process chunk → signal done ───┼→ all done
                    ├─── Worker 2: Condition.wait → process chunk → signal done ───┤
                    └─── Main domain: process chunk 0 → wait for workers ──────────┘
```

- `WorkerPool.create nw`: spawns `nw` worker domains that immediately
  block on `Condition.wait`. Zero CPU.
- `WorkerPool.parallel_for pool length f`: partitions `[0..length-1]`
  into contiguous chunks across `nw+1` domains (workers + main). Workers
  are woken via `Condition.broadcast`, process their chunk, then signal
  completion via a counter + `Condition.signal`.
- `WorkerPool.teardown pool`: sets a `stop` flag, broadcasts, joins all
  worker domains.
- **Nested call detection:** An `Atomic.t bool` `busy` flag prevents
  nested `parallel_for` calls (e.g. analyzer iterating types → iterating
  fields) from corrupting shared state. Nested calls fall back to
  sequential execution, matching Domainslib's effective behaviour for the
  same code paths.
- **Exception propagation:** First exception from any domain (worker or
  main) is captured with backtrace and re-raised after all domains finish.

**`ManagedPool`** is retained as a thin wrapper that lazily creates a
`WorkerPool` on first use and tears it down on `release`. Workers sleep
between `run_with_pool` scopes — zero CPU overhead.

**Benchmark results** (4-core CI runner, median of 5 runs for eval, 3 for JVM):

| Benchmark | Domainslib | WorkerPool | Delta |
|---|---|---|---|
| Eval unit tests | 2762 ms | 2809 ms | +1.7 % (noise) |
| JVM compilation | 1062 ms | 1070 ms | +0.8 % (noise) |

Throughput is within measurement noise — the WorkerPool is not faster for
active parallel work. The primary benefit is eliminating idle CPU
consumption: Domainslib workers spin-wait on a lock-free channel even when
no work is available, whereas WorkerPool workers block on `Condition.wait`
and consume zero CPU between parallel sections. This matters for the
compilation server where the pool persists across requests.
