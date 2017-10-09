[![Travis CI](https://img.shields.io/travis/knutwalker/validation/master.svg)](https://travis-ci.org/knutwalker/validation)
[![Coverage Status](https://img.shields.io/codecov/c/github/knutwalker/validation/master.svg)](https://codecov.io/github/knutwalker/validation)
[![Maven](https://img.shields.io/maven-central/v/de.knutwalker/validation_2.11.svg)](http://search.maven.org/#search|ga|1|g%3A%22de.knutwalker%22%20AND%20a%3A%22validation_2.11%22)
[![Apache License](https://img.shields.io/badge/license-APACHE_2-green.svg)](https://www.apache.org/licenses/LICENSE-2.0)

## Validation

A standalone Validation data type, called `Result`. (see [How do I error handle thee?](http://typelevel.org/blog/2014/02/21/error-handling.html) for more on the general idea of a Validation)

The `Result` type is very similar to `scalaz.Validation`, `cats.data.Validated`, or `org.scalactic.Or` with the following differences:

- no dependencies besides scala and no additional baggage

that is no Monads, Applicative Functors, Arrows, or Categories.
The only thing that is provided besides `Result` is a `NonEmptyVector` which is
similar to a regular `Vector`, only that there will always be at least one element in it.

The main incentive is, that `Result` can be used in places where a full-fledged
dependency on `scalaz` and co. is undesired, e.g. in projects with many people
that are unfamiliar with or intimidated by `scalaz`.

- explicit symbolic and unsafe operators

By default, all operations are safe, meaning they are referentially transparent if their input is.
Unsafe operations (`foreach`, `get`) can be enabled by
importing `validation.Result.unsafe._`

All methods are named aptly (or so I hope) and with ascii characters.
Symbolic names (such as `|` for `getOrElse` or `|@|` for `and`) can be enabled by
importing `validation.Result.symbolic._`

- implied underlying Vector to accumulate data

Scalaz differentiates between `Validation[E, A]` and `type ValidationNel[E, A] = Validation[NonEmptyList[E], A]`
where as `Result[E, A]` has an implied `NonEmptyVector` accumulating the `E`.
It behaves mostly as a permanent (imaginary) `ResultNev[E, A]`.

### What Validation is not

This library is not an implementation of a validation framework, just
a general data structure.


## Installing

```
// scala 2.11.x
libraryDependencies += "de.knutwalker" %% "validation" % "0.2.0"

// scala 2.12.x
libraryDependencies += "de.knutwalker" %% "validation" % "0.3.0"
```


## Usage

```scala
import validation._

type Validated[A] = Result[String, A]

case class Person(name: String, age: Int)

def parseName(s: String): Validated[String] =
  if (s.trim.nonEmpty) Result.valid(s.trim)
  else Result.invalid(s"invalid name: '$s'")

def parseAge(s: String): Validated[Int] =
  Result.catching[NumberFormatException]
    .using(_.getMessage)
    .run(s.trim.toInt)
    .filter(_ >= 0, s"invalid age: '$s'")

def parsePerson(name: String, age: String): Validated[Person] =
   (parseName(name) and parseAge(age)) apply Person

parsePerson("Bernd", "42")
// res0: Validated[Person] = Valid(Person(Bernd,42))

parsePerson("Bernd", "fortytwo")
// res1: Validated[Person] = Invalid(For input string: "fortytwo")

parsePerson("Bernd", "-1337")
// res2: Validated[Person] = Invalid(invalid age: '-1337')

parsePerson("", "")
// res3: Validated[Person] = Invalids(NonEmptyVector(invalid name: '',For input string: ""))
```
