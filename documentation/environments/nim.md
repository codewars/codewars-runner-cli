# Environment

Code is executed within a Dockerized Ubuntu 14.04 container.

## Language

[Nim](https://nim-lang.org/) [v0.17.0 (2017-05-17)](https://nim-lang.org/blog/2017/05/17/version-0170-released.html)

## Testing

[unittest](https://nim-lang.org/docs/unittest.html) module is used.

"Preloaded Code" (`opts.setup`) is written to `setup.nim` in the same directory.

```nim
# solution.nim
# can `import setup`
proc add*(x, y: int): int = x + y
```

Tests can access exported identifiers in the solution.

```nim
# fixture.nim
suite "add":
  test "1 + 1 = 2":
    check(add(1, 1) == 2)
```

The following code is used to coordinate tests:

```nim
import unittest, codewars/formatter
addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))
import solution
include fixture
```
