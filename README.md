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

Bounded `list<s64>` payloads additionally validate item count, alignment,
pointer overflow, and arena range in the selected case, then alias the admitted
input buffer until canonical post-return resets the arena.

Other list item types, nested option/result payloads, and recursive records
remain fail-closed pending recursive element validation and linearity analysis.

## Test

```bash
clojure -M:test
```
