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

## Test

```bash
clojure -M:test
```
