# kotoba-component

Wasm Component production — lifts core modules through the Canonical ABI.

**Tier**: `T2`  **Role**: `backend`

Split out of the overloaded core repos by ADR-2607266000 so that each
responsibility has exactly one owner and the dependency direction is
checkable from outside.

## Owns

- `kotoba.component.core`
- `kotoba.component.composition`
- `kotoba.component.admission`
- `kotoba.component.artifact`
- `kotoba.component.wit`

## Does not own

- parse .kotoba source
- execute components
- decide grants

## Depends on

- `kotoba-lang/kotoba-kir`
- `kotoba-lang/kotoba-wasm`

## Structural union boundary

`option<T>` and `result<T, E>` identity exports use the same active-case
Canonical ABI validation as sealed variants. Payloads may be canonical
scalars, bounded strings/keywords, or finite sealed records recursively
containing those leaves. Validation is case-dependent: malformed inactive
joined slots are ignored, while malformed leaves in the selected case trap.

Bounded `list<s64>` and `list<f64>` payloads additionally validate item
count, alignment, pointer overflow, and arena range in the selected case, then
alias the admitted input buffer until canonical post-return resets the arena.

Nested `option`/`result` payloads recursively validate each inner
discriminant and only the selected inner case before storing the same nested
in-memory union shape. Other list item types and recursive records remain
fail-closed pending per-element validation and linearity analysis.

Non-identity `option`/`result` matches can consume scalar payloads and finite
records whose recursive leaves are `s64`, `float32`, `float64`, or `bool`.
Record binders remain sealed: branch code may access them only through a
statically resolved `record-get` chain to an admitted scalar leaf. Every bool
leaf in the selected case is validated even when the branch does not read it;
inactive joined slots are never interpreted. Branch expressions still use the
shared typed core-Wasm emitter rather than a Component-specific compiler.

Those match modules may also call scalar named capabilities. Every
`typed-cap-call` must resolve to an explicit standard32 WIT import; canonical
adapters never fall back to the generic host ABI. The match adapter, ordinary
scalar helpers, fuel global, and capability calls remain in one core module,
so composition does not introduce a second expression compiler or ambient
WASI authority.

Selected string/keyword leaves may feed `string-byte-length` without becoming
host objects. The shared core emitter consumes their Canonical `(ptr,len)`
slots and checks the declared byte bound, unsigned end-pointer overflow, and
the module's actual memory size. Every selected indirect leaf is validated
even when branch code ignores it; inactive union slots remain uninterpreted.
Raw indirect binders and unrelated string operations remain fail-closed.

## Test

```bash
clojure -M:test
```
