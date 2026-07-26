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
scalars, sealed flat scalar records, or sealed flat records containing bounded
string/keyword leaves. Validation is case-dependent: malformed inactive joined
slots are ignored, while malformed leaves in the selected case trap.

Nested records and list payloads remain fail-closed until the case-leaf walker,
ownership cleanup, and linearity analysis recurse into those shapes.

## Test

```bash
clojure -M:test
```
