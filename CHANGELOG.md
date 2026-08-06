# v1.2.3

- Conform to the new `hasql` API (v2.0)

# v1.2

- Removed the `unpreparedTransaction` session because the same effects can now be achieved via the connection settings in Hasql

# v1.1

- Add automatic retry on deadlock errors (code 40P01)
