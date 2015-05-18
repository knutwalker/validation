# Change Log

## Versioning

Starting with `1.0`, this project will follow [Semantic Versioning](http://semver.org/).
While on version `0.x`, breaking and incompatible changed may be introduced
by a `MINOR` version bump as well. More precisely, semantic versioning is shifted
one version to the right, so it is considered `0.MAJOR.MINOR`.


## [Unreleased][unreleased]

## [0.2.0][0.2.0] - 2015-05-18
### Added
- `NonEmptyVector` that represents a Vector that is guaranteed to be non-empty
    - `NEV` is now used to accumulate any invalid value
    - User now need to only provide the invalid type, not the desired wrapper type
      (e.g. just `String` instead of `List[String]`)
- type alias `E \?/ A` for `Result[E, A]` (needs to be imported explicitly) 
- lots of documentation and tests

##### Methods on trait Result

- `as` and `void` to replace or discard the valid values, respectively
- `recoverAll` to recover from all invalids, not just the latest one
- `recoverWith` to recover with another validation
- `nev` to wrap the valid value in a `NonEmptyVector`
- `getEither` for when both types are interchangeable

##### Constructors on object Result
- `Result.catching` can be further configured with `using` to apply a function to the exception
- `cond` as a ternary operator in the Result context
- various `parseXXX` methods to parse a string into a primitive value
- `traverse_` for discarding any valid value while traversing

### Changed
- curried fold (`invalid`/`valid`) now uses a curried method instead of an inner class

### Removed
- `Mergeable` typeclass removed.
   The functionality of accumulating invalid values is taken over by
   the implied `NonEmptyVector` on the invalid side.
- Support for Scala 2.10, possibly only temporary

### Fixed
- `and` combinator was only available when combining at most 4 results, now goes up to 9 

## [0.1.0][0.1.0] - 2015-05-06
### Added
- Initial release for validation


[unreleased]: https://github.com/knutwalker/validation/compare/v0.2.0...develop
[0.2.0]: https://github.com/knutwalker/validation/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/knutwalker/validation/compare/5956c75...v0.1.0
