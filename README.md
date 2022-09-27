# AtLeast

Integers and data structures that are at least some minimum size. Currently
contains:

- `IntAL n`: an integer whose run-time value is greater than or equal to
             type-level integer
- `ArrayAL n`: an array that contains at least the type-level specified number
               of elements. `ArrayAL 0` is isomorphic to `Array` and `ArrayAL 1`
               is isomorphic to `NonEmptyArray`. The length of an `ArrayAL n`
               is an `Int n`

`IntAL n` supports basic arithmetic with operators %+%, %-%, %*% and %/% as well
as `gcd` and `lcm`. Conversion between `IntAL n` and `Int m` and conversion to
and from a plain `Int` are also supported, of course.

`ArrayAL n` supports many of the functions available on `Array` but with tighter
types where possible. There is also the function `selfZipWith` which applies
a function to two offset copies of an `ArrayAL`. To keep the dependency list
short, this library does not refer to `TwoOrMore`, but nevertheless `ArrayAL 2`
is isomorphic to `TwoOrMore`

Tasks remaining:

- Finish out functions from `Array`
- Tests, particularly for `ArrayAL`
- Implement other datastructures with minimum sizes, possibly including `Set`,
  `Map`, `List`
