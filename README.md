[![Travis CI](https://img.shields.io/travis/knutwalker/validation/master.svg)](https://travis-ci.org/knutwalker/validation)
[![Coverage Status](https://img.shields.io/coveralls/knutwalker/validation/master.svg)](https://coveralls.io/r/knutwalker/validation)
[![Maven](https://img.shields.io/maven-central/v/de.knutwalker/validation_2.11.svg)](http://search.maven.org/#search|ga|1|g%3A%22de.knutwalker%22%20AND%20a%3A%22validation_2.11%22)
[![Apache License](https://img.shields.io/badge/license-APACHE_2-green.svg)](https://www.apache.org/licenses/LICENSE-2.0)

## Validation

A standalone Validation data type, called `Result`. (see [How do I error handle thee?](http://typelevel.org/blog/2014/02/21/error-handling.html) for more on the general idea of a Validation)

The `Result` type is very similar to `scalaz.Validation`, `cats.data.Validated`, or `org.scalactic.Or` with the following differences:

- no dependencies besides scala and no additional baggage

that is no Monads, Applicative Functors, Arrows, or Categories.
The only thing that is provided besides `Result` is a type class called `Mergeable`
which is a Semigroup in disguise. Plans to remove even this exist.

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


### What Validation is not

This library is not an implementation of a validation framework, just
a general data structure.


## Installing

```
libraryDependencies += "de.knutwalker" %% "validation" % "0.1.0"
```


## Usage

```scala
import validation._

type Validated[A] = Result[List[String], A]

case class Person(name: String, age: Int)

def parseName(s: String): Validated[String] =
  if (s.trim.nonEmpty) Result.valid(s.trim)
  else Result.invalid(List(s"invalid name: '$s'"))

def parseAge(s: String): Validated[Int] =
  Result.catching[NumberFormatException].run(s.trim.toInt)
    .invalidMap(x => List(x.getMessage))
    .filter(age => age >= 0, List(s"invalid age: '$s'"))

def parsePerson(name: String, age: String): Validated[Person] =
  (parseName(name) and parseAge(age)) apply Person

parsePerson("Bernd", "42")
// res0: Validated[Person] = Valid(Person(Bernd,42))

parsePerson("Bernd", "fortytwo")
// res1: Validated[Person] = Invalid(List(For input string: "fortytwo"))

parsePerson("Bernd", "-1337")
// res2: Validated[Person] = Invalid(List(invalid age: '-1337'))

parsePerson("", "")
// res3: Validated[Person] = Invalid(List(invalid age: '', invalid name: ''))
```