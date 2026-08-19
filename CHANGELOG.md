# v1.2.3.1

- Update test dependencies

# v1.2.3.0

## Non-breaking

- Add `Semigroup`/`Monoid` instances for `Mode` and `IsolationLevel`, combining via `max` with `mempty` as the weakest value, so composed transaction requirements escalate to the strictest one explicitly requested.

# v1.2.2.1

- Conform to the new `hasql` API (v2.0)

# v1.2

- Removed the `unpreparedTransaction` session because the same effects can now be achieved via the connection settings in Hasql

# v1.1

- Add automatic retry on deadlock errors (code 40P01)
