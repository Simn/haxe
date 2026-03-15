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

**Key call sites in hot paths:**

- **`src/typing/typeloadCheck.ml:187`** — `| a, b when a = b -> ()` comparing
  `field_kind` values (contains `var_access` / `method_kind`).
- **`src/typing/typeloadCheck.ml:432`** — `mkind m1 = mkind m2` comparing
  method kinds.
- **`src/typing/typeloadCheck.ml:66`** — `not (m1 = MethDynamic)` comparing
  method kinds.
- **`src/typing/fields.ml:41`** — `!(a.a_status) = Const` comparing
  `anon_status`.
- **`src/typing/fields.ml:141,194`** — `e.eexpr = TConst TSuper` comparing
  `texpr_expr` constructors (cheap since TConst is simple).
- **`src/optimization/optimizerTexpr.ml:189`** — `a = b` comparing arbitrary
  constants.
- **Path comparisons** (10 sites) — `c.cl_path = path` etc. These compare
  `string list * string` tuples, which is relatively cheap.
- **Hashtbl operations** — `Hashtbl.find`/`Hashtbl.mem` use polymorphic
  hashing and equality by default. `nullSafety.ml` has 26 such call sites.

**Recommendation:** Replace polymorphic `(=)` with typed equality in the
hottest paths, especially `typeloadCheck.ml`. For `Hashtbl`, consider using
functorized hash tables with custom hash/equality for type keys.

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

### 5. Domainslib Domain Management

Worker domains spin-wait on `Multi_channel.recv_poll_loop` even for small
compilations. The `ManagedPool` in `parallel.ml` already supports lazy
acquisition and teardown, but the pool is created at the start of
compilation regardless of workload size.

**Recommendation:** The Domainslib API creates a fixed pool of domains.
A better approach would be to use OCaml 5's `Domain.spawn` directly for
short parallel sections (like HXB export) instead of maintaining a
persistent pool. This avoids idle spin-waiting entirely. Alternatively,
gate pool creation behind a module-count threshold (e.g., only create
the pool when there are > 50 modules to process in parallel).
