/*
 * Copyright 2015 Paul Horn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package validation

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag
import scala.util.control.Exception
import scala.util.{Failure, Success, Try}

/**
 * Represents a value of one of two states:
 *  - `[[validation.Result.Valid Valid]](a)`
 *  - `[[validation.Result.Invalid Invalid]](e)`
 *
 * It is similar to [[scala.util.Either Either]] or a more generic [[scala.util.Try Try]].
 *
 * == Motivation ==
 *
 * Scalas `Either` is unbiased, meaning it treats its two sides equally and
 * requires one to explicitly state on which side one wants to work on.
 * More often that not, one side is representing the error or invalid part and
 * one side representing the success or valid part.
 *
 * `Result` sets the invalid side on the left and the valid side on the right
 * and assumes, that one wants to primarily work on the right/valid side.
 * This is similar to `Try`, which fixes the invalid type to `Throwable`.
 *
 * Example:
 * {{{
 * import validation._, Result._
 *
 * case class Person(name: String)
 *
 * def validateName(input: String): Result[String, String] =
 *   if (input.trim.isEmpty)
 *     invalid("name must not be empty")
 *   else
 *     valid(input)
 *
 * List("Bernd", "").map(validateName(_).map(Person.apply))
 *
 * // List[Result[String,Person]] = List(
 * //   Valid(Person(Bernd)),
 * //   Invalid(name must not be empty)
 * // )
 * }}}
 *
 * Both, `Either` and `Try`, cannot accumulate errors, but are typically
 * aborting with the first failure, making them disadvantageous for parsing and
 * validating data on system boundaries.
 * For example, parsing a JSON data structure can lead to multiple issues and
 * it is good measure to report as many errors as you can back to the user.
 * With `Either` or `Try`, only one error at the time gets reported, leading
 * to the user to submit a fixed version only to encounter, that they have to
 * fixed yet another error.
 *
 * `Result` can accumulate errors and thus allows to report as many of them as
 * possible. There are multiple ways to combine two or more `Result`s, for a
 * detailed description, see their respective documentation.
 *
 * Example:
 * {{{
 * import validation._, Result._
 *
 * case class Person(name: String, age: Int)
 *
 * def validateName(input: String): Result[String, String] =
 *   if (input.trim.isEmpty)
 *     invalid("name must not be empty")
 *   else
 *     valid(input)
 *
 * def validateAge(input: String): Result[String, Int] = {
 *   val ageVal =
 *     if (input.trim.isEmpty)
 *       invalid("age must not be empty")
 *     else
 *       Result.parseInt(input).invalidMap(_.getMessage)
 *   ageVal.filter(_ >= 0, "age must be positive")
 * }
 *
 * def parsePerson(inName: String, inAge: String): Result[List[String], Person] = {
 *   val nameVal = validateName(inName).invalidMap(List(_))
 *   val ageVal = validateAge(inAge).invalidMap(List(_))
 *   (nameVal and ageVal) apply Person
 * }
 *
 * val inputs = List(
 *   ("Bernd", "42"),
 *   ("Ralle", ""),
 *   ("Ronny", "foo"),
 *   ("", "-1337")
 * )
 *
 * inputs.map((parsePerson _).tupled)
 *
 * // List[Result[List[String],Person]] = List(
 * //   Valid(Person(Bernd,42)),
 * //   Invalid(List(age must not be empty)),
 * //   Invalid(List(For input string: "foo")),
 * //   Invalid(List(name must not be empty, age must be positive))
 * // )
 * }}}
 *
 *
 * @groupname Access Accessors and Extractors
 * @groupdesc Access Methods to access or extract a value from this Result.
 * @groupprio Access 1
 *
 * @groupname Transform Transformers
 * @groupdesc Transform Methods to transform the value of this Result.
 * @groupprio Transform 2
 *
 * @groupname Test Testers
 * @groupdesc Test Methods to test the value of this Result for some properties.
 * @groupprio Test 3
 *
 * @groupname Translate Translators
 * @groupdesc Translate Methods to translate this Result into a different structure.
 * @groupprio Translate 4
 *
 * @groupname Combine Combinators
 * @groupdesc Combine Methods to combine multiple Results.
 * @groupprio Combine 5
 *
 * @tparam E the `Invalid` type
 * @tparam A the `Valid` type
 * @since 0.1.0
 */
sealed trait Result[+E, @specialized +A] extends Any with Product with Serializable {

  /**
   * Folds this `Result` into a value by applying the first function if this
   * is a `Invalid` or the second function if this is a `Valid`.
   *
   * This is the Catamorphism.
   *
   * @group Access
   * @param fe the function to apply in the invalid case
   * @param fa the function to apply in the valid case
   * @tparam B the resulting type
   * @return the result of one of the two functions
   */
  def fold[B](fe: NonEmptyVector[E] ⇒ B, fa: A ⇒ B): B = this match {
    case Result.Invalid(e)   ⇒ fe(NonEmptyVector(e))
    case Result.Invalids(es) ⇒ fe(es)
    case Result.Valid(a)     ⇒ fa(a)
  }

  /**
   * Curried [[fold]] that starts with the valid part.
   * @see [[fold]]
   * @group Access
   * @param fa the function to apply in the valid case
   * @param fe the function to apply in the invalid case
   * @tparam B the resulting type
   * @return the result of one of the two functions
   */
  def valid[B](fa: A ⇒ B)(fe: NonEmptyVector[E] ⇒ B): B =
    fold(fe, fa)

  /**
   * Curried [[fold]] that starts with the invalid part.
   * @see [[fold]]
   * @group Access
   * @param fe the function to apply in the invalid case
   * @param fa the function to apply in the valid case
   * @tparam B the resulting type
   * @return the result of one of the two functions
   */
  def invalid[B](fe: NonEmptyVector[E] ⇒ B)(fa: A ⇒ B): B =
    fold(fe, fa)

  /**
   * A left fold over the valid value.
   * @see [[fold]]
   * @group Access
   * @param b the initial accumulator
   * @param f reducer function of the accumulator and the valid value
   * @tparam B the resulting type
   * @return `b` if this is `Invalid`, otherwise the result of applying `f`
   */
  def foldLeft[B](b: B)(f: (B, A) ⇒ B): B =
    fold(_ ⇒ b, f(b, _))

  /**
   * A right fold over the valid value.
   * @see [[fold]]
   * @group Access
   * @param b the initial accumulator
   * @param f reducer function of the valid value and the accumulator
   * @tparam B the resulting type
   * @return `b` if this is `Invalid`, otherwise the result of applying `f`
   */
  def foldRight[B](b: B)(f: (A, B) => B): B =
    fold(_ ⇒ b, f(_, b))

  /**
   * Transforms the value of this `Result`.
   * @group Transform
   * @param fe the function to transform an invalid value
   * @param fa the function to transform a valid value
   * @tparam EE the resulting invalid type
   * @tparam AA the resulting valid type
   * @return the result with either the valid or invalid value transformed
   */
  def bimap[EE, AA](fe: E ⇒ EE, fa: A ⇒ AA): Result[EE, AA] =
    fold(es ⇒ Result.invalids(es.map(fe)), fa andThen Result.valid)

  /**
   * Maps over the valid value of this `Result`.
   *
   * This is the Functor over the valid side.
   *
   * @group Transform
   * @param f the function to apply if this `Result` is valid
   * @tparam B the resulting valid type
   * @return the result of the function application in the `Result` context
   */
  def map[B](f: A ⇒ B): Result[E, B] =
    bimap(identity, f)

  /**
   * Maps over the invalid value of this `Result`.
   *
   * This is the Functor over the invalid side.
   *
   * @group Transform
   * @param f the function to apply if this `Result` is invalid
   * @tparam F the resulting invalid type
   * @return the result of the function application in the `Result` context
   */
  def invalidMap[F](f: E ⇒ F): Result[F, A] =
    bimap(f, identity)

  /**
   * Sets the valid value to `x` and discards the previous value
   * if this is a valid Result.
   *
   * @group Transform
   * @param x the new valid value
   * @tparam B the resulting valid type
   * @return the result with the valid value set to `x`
   * @since 0.2.0
   */
  def as[B](x: ⇒ B): Result[E, B] =
    map(_ ⇒ x)

  /**
   * Discards the valid value if this is a valid Result.
   *
   * @group Transform
   * @return the result with the valid value set to Unit
   * @since 0.2.0
   */
  def void: Result[E, Unit] =
    as(())

  /**
   * Continues validation with the provided function
   * if this is a valid `Result`.
   *
   * `flatMap` does not accumulate errors.
   * If you want to do so, use `and` instead.
   *
   * This is the Monadic Bind through the valid value.
   *
   * @group Transform
   * @param f the function to continue with
   * @tparam B the resulting valid type
   * @return the result of applying `f` over the valid value in the `Result` context
   *
   * @usecase def flatMap[B](f: A => Result[E, B]): Result[E, B]
   *          @inheritdoc
   */
  def flatMap[EE >: E, B](f: A ⇒ Result[EE, B]): Result[EE, B] =
    fold(Result.invalids, f)

  /**
   * Transforms an invalid Result into a valid one.
   *
   * @group Transform
   * @param f the function to transform an invalid value into a valid one
   * @return a result that is always valid
   * @usecase def recover(f: E ⇒ A): Result[E, A]
   *          @inheritdoc
   */
  def recover[AA >: A](f: E ⇒ AA): Result[E, AA] =
    fold(es ⇒ Result.valid(f(es.head)), _ ⇒ this)

  /**
   * Transforms an invalid Result into a valid one.
   *
   * @group Transform
   * @param f the function to transform all invalid values into a valid one
   * @return a result that is always valid
   * @since 0.2.0
   * @usecase def recoverAll(f: NonEmptyVector[E] ⇒ A): Result[E, A]
   *          @inheritdoc
   */
  def recoverAll[AA >: A](f: NonEmptyVector[E] ⇒ AA): Result[E, AA] =
    fold(es ⇒ Result.valid(f(es)), _ ⇒ this)

  /**
   * Continues validation with the provided function
   * if this is an invalid `Result`.
   *
   * This is the Monadic Bind through the invalid value.
   *
   * @group Transform
   * @param f the function to continue with
   * @tparam F the resulting invalid type
   * @return the result of applying `f` over the invalid value in the `Result` context
   * @since 0.2.0
   * @usecase def recoverWith[F](f: E ⇒ Result[F, A]): Result[F, A]
   *          @inheritdoc
   */
  def recoverWith[AA >: A, F](f: E ⇒ Result[F, AA]): Result[F, AA] =
    fold(_.reduce(f)(_ orElse _), Result.valid)

  /**
   * @group Transform
   * @return Result where the valid value is wrapped in a [[NonEmptyVector]]
   * @since 0.2.0
   */
  def nev: Result[E, NonEmptyVector[A]] =
    map(NonEmptyVector(_))

  /**
   * Applies a function in the `Result` context to the valid value,
   * accumulating invalids.
   *
   * This is the Applicative Functor of the valid Result.
   *
   * @group Combine
   * @param f the function in the `Result` context
   * @tparam B the resulting valid type
   * @return the valid result if `this` and `f` are valid
   *         or invalid if one of each is invalid
   *         or both invalid values accumulated
   * @usecase def apply[B](f: Result[E, A ⇒ B]): Result[E, B]
   *          @inheritdoc
   */
  def apply[EE >: E, B](f: Result[EE, A ⇒ B]): Result[EE, B] =
    fold(
      e1 ⇒ f.fold(e2 ⇒ Result.invalids(e2 ++ e1), _ ⇒ Result.invalids(e1)),
      a1 ⇒ f.fold(Result.invalids, ab ⇒ Result.valid(ab(a1)))
    )

  /**
   * Combines two `Result`s in a builder, eventually accumulating invalids.
   *
   * This is the Applicative Builder of the valid result.
   *
   * @group Combine
   * @param other the other `Result` this one should be combined with
   * @tparam B the other valid type
   * @return a builder to eventually apply two `Result`s
   * @usecase def and[B](other: Result[E, B]): Result.Ap2[E, A, B]
   *          @inheritdoc
   */
  def and[EE >: E, AA >: A, B](other: Result[EE, B]): Result.Ap2[EE, AA, B] =
    new Result.Ap2(this, other)

  /**
   * Combines two `Result` by zipping their valid values together,
   * accumulating errors.
   *
   * @group Combine
   * @param b
   * @tparam B
   * @return
   * @usecase def zip[B](b: Result[E, B]): Result[E, (A, B)]
   *          @inheritdoc
   */
  def zip[EE >: E, B](b: Result[EE, B]): Result[EE, (A, B)] =
    and(b).tupled

  /**
   * Filters the valid value of this `Result`.
   *
   * @group Test
   * @param p
   * @param ifEmpty
   * @return
   * @usecase def filter(p: A ⇒ Boolean, ifEmpty: ⇒ E): Result[E, A]
   *          @inheritdoc
   */
  def filter[EE >: E](p: A ⇒ Boolean, ifEmpty: ⇒ EE): Result[EE, A] =
    fold(_ ⇒ this, a ⇒ if (p(a)) this else Result.invalid(ifEmpty))

  /**
   * @group Test
   * @return `true` if this is a valid `Result`
   */
  def isValid: Boolean =
    fold(_ ⇒ false, _ ⇒ true)

  /**
   * @group Test
   * @return `true` if this is an invalid `Result`
   */
  def isInvalid: Boolean =
    !isValid

  /**
   * @group Test
   * @param p
   * @return `true` if this `Result` is valid and satisfies the predicate `p`
   */
  def exists(p: A ⇒ Boolean): Boolean =
    fold(_ ⇒ false, p)

  /**
   * @group Test
   * @param p
   * @return `true` if this `Result` is invalid or the valid value satisfies the predicate `p`
   */
  def forall(p: A ⇒ Boolean): Boolean =
    fold(_ ⇒ true, p)

  /**
   * Tests for membership on the valid `Result`
   * @group Test
   * @param x
   * @return `true` if this `Result` has `x` as valid value
   * @usecase def contains(x: A): Boolean
   *          @inheritdoc
   */
  def contains[AA >: A](x: ⇒ AA): Boolean =
    fold(_ ⇒ false, _ == x)

  /**
   * @group Translate
   * @return a [[scala.Option]]
   */
  def toOption: Option[A] =
    fold(_ ⇒ None, Some.apply)

  /**
   * @group Translate
   * @return
   */
  def toSeq: immutable.Seq[A] =
    fold(_ ⇒ immutable.Seq.empty, immutable.Seq(_))

  /**
   * @group Translate
   * @return
   */
  def toList: List[A] =
    fold(_ ⇒ Nil, List(_))

  /**
   * @group Translate
   * @return
   */
  def toStream: Stream[A] =
    fold(_ ⇒ Stream.empty, Stream(_))

  /**
   * @group Translate
   * @return
   */
  def toVector: Vector[A] =
    fold(_ ⇒ Vector.empty, Vector(_))

  /**
   * @group Translate
   * @return
   * @usecase def toSet: Set[A]
   *          @inheritdoc
   */
  def toSet[AA >: A]: Set[AA] =
    fold(_ ⇒ Set.empty, Set(_))

  /**
   * @group Translate
   * @return
   */
  def toEither: Either[NonEmptyVector[E], A] =
    fold(Left.apply, Right.apply)

  /**
   * @group Translate
   * @param ev
   * @return
   * @usecase def toTry: Try[A]
   */
  def toTry(implicit ev: E <:< Throwable): Try[A] =
    fold(es ⇒ Failure(es.head), Success.apply)

  /**
   * @group Transform
   * @return
   */
  def swap: Result[A, NonEmptyVector[E]] =
    fold(Result.valid, Result.invalid)

  /**
   * @group Transform
   * @param f
   * @tparam EE
   * @tparam AA
   * @return
   */
  def swapped[EE, AA](f: Result[A, NonEmptyVector[E]] ⇒ Result[AA, NonEmptyVector[EE]]): Result[EE, NonEmptyVector[AA]] =
    f(swap).fold(Result.valid, Result.invalids)

  /**
   * @group Access
   * @param aa
   * @return
   * @usecase def getOrElse(a: A): A
   *          @inheritdoc
   */
  def getOrElse[AA >: A](aa: ⇒ AA): AA =
    fold(_ ⇒ aa, identity)

  /**
   * @group Access
   * @param x
   * @return
   * @usecase def valueOr(x: E ⇒ A): A
   *          @inheritdoc
   */
  def valueOr[AA >: A](x: NonEmptyVector[E] ⇒ AA): AA =
    fold(x, identity)

  /**
   * @group Access
   * @param ev
   * @return
   * @since 0.2.0
   * @usecase def getEither: A
   *          @inheritdoc
   */
  def getEither[AA >: A](implicit ev: E <:< AA): AA =
    fold(_.head, identity)

  /**
   * @group Combine
   * @param other
   * @return
   * @usecase def orElse(other: Result[E, A]): Result[E, A]
   *          @inheritdoc
   */
  def orElse[EE >: E, AA >: A](other: ⇒ Result[EE, AA]): Result[EE, AA] =
    fold(_ ⇒ other, _ ⇒ this)

  /**
   * @group Combine
   * @param other
   * @return
   * @usecase def merge(other: Result[E, A]): Result[E, A]
   *          @inheritdoc
   */
  def merge[EE >: E, AA >: A](other: ⇒ Result[EE, AA]): Result[EE, NonEmptyVector[AA]] =
    fold(
      e1 ⇒ other.fold(e2 ⇒ Result.invalids(e2 ++ e1), _ ⇒ this.nev),
      a1 ⇒ other.fold(_ ⇒ other.nev, a2 ⇒ Result.valid(NonEmptyVector.of(a1, a2)))
    )

  /**
   * @group Combine
   * @param other
   * @return
   * @usecase def append(other: Result[E, A]): Result[E, A]
   *          @inheritdoc
   */
  def append[EE >: E, AA >: A](other: ⇒ Result[EE, AA]): Result[EE, NonEmptyVector[AA]] =
    fold(
      e1 ⇒ other.fold(e2 ⇒ Result.invalids(e2 ++ e1), _ ⇒ other.nev),
      a1 ⇒ other.fold(_ ⇒ this.nev, a2 ⇒ Result.valid(NonEmptyVector.of(a1, a2)))
    )

  /**
   * @param other
   * @param EE
   * @param AA
   * @return
   * @usecase def compare(other: Result[E, A]): Int
   *          @inheritdoc
   */
  def compare[EE >: E, AA >: A](other: Result[EE, AA])(implicit EE: Ordering[EE], AA: Ordering[AA]): Int =
    fold(
      e ⇒ other.fold(e.compare(_), _ ⇒ -1),
      a ⇒ other.fold(_ ⇒ 1, AA.compare(a, _))
    )

  /**
   * @param other
   * @param EE
   * @param AA
   * @return
   * @usecase def ==(other: Result[E, A]): Boolean
   *          @inheritdoc
   */
  def ==[EE >: E, AA >: A](other: Result[EE, AA])(implicit EE: Equiv[EE], AA: Equiv[AA]): Boolean =
    fold(
      e ⇒ other.fold(_ == e, _ ⇒ false),
      a ⇒ other.fold(_ ⇒ false, AA.equiv(a, _))
    )
}

/**
 * Companion object for [[Result]]
 *
 * provides many factories and creators for `Result` instances
 *
 * @groupname Create Factories and Constructors
 * @groupdesc Create Methods to create new Results.
 * @groupprio Create 1
 *
 * @groupname Combine Combinators
 * @groupdesc Combine Methods to combine multiple Results.
 * @groupprio Combine 2
 *
 * @groupname Unsafe Unsafe
 * @groupdesc Unsafe Methods that are referentially unsafe (have side-effects).
 * @groupprio Unsafe 3
 *
 * @groupname Symbolic Symbolic Aliases
 * @groupdesc Symbolic Symbolic aliases to existing methods.
 * @groupprio Symbolic 4
 *
 * @since 0.1.0
 */
object Result {

  /**
   * @group Create
   * @param x
   * @tparam E
   * @tparam A
   * @return
   */
  def valid[E, A](x: A): Result[E, A] =
    new Valid[A](x)

  /**
   * @group Create
   * @param e
   * @tparam E
   * @tparam A
   * @return
   */
  def invalid[E, A](e: E): Result[E, A] =
    new Invalid[E](e)

  /**
   * @group Create
   * @param es
   * @tparam E
   * @tparam A
   * @return
   */
  def invalids[E, A](es: NonEmptyVector[E]): Result[E, A] =
    if (es.tail.isEmpty) new Invalid[E](es.head) else new Invalids[E](es)

  /**
   * @group Create
   * @param x
   * @tparam A
   * @return
   */
  def fromTry[A](x: Try[A]): Result[Throwable, A] = x match {
    case Failure(e) ⇒ invalid(e)
    case Success(a) ⇒ valid(a)
  }

  /**
   * @group Create
   * @param x
   * @tparam E
   * @tparam A
   * @return
   */
  def fromEither[E, A](x: Either[E, A]): Result[E, A] = x match {
    case Left(e)  ⇒ invalid(e)
    case Right(a) ⇒ valid(a)
  }

  /**
   * @group Create
   * @param x
   * @param ifNone
   * @tparam E
   * @tparam A
   * @return
   */
  def fromOption[E, A](x: Option[A], ifNone: ⇒ E): Result[E, A] = x match {
    case None    ⇒ invalid(ifNone)
    case Some(a) ⇒ valid(a)
  }

  /**
   * @group Create
   * @tparam T
   * @return
   */
  def catching[T >: Null <: Throwable : ClassTag]: FromTryCatchAux[T, T] =
    new FromTryCatchAux[T, T](identity)

  /**
   * @group Create
   * @param p
   * @param ifTrue
   * @param ifFalse
   * @tparam E
   * @tparam A
   * @return
   * @since 0.2.0
   */
  def cond[E, A](p: Boolean, ifTrue: ⇒ A, ifFalse: ⇒ E): Result[E, A] =
    if (p) valid(ifTrue) else invalid(ifFalse)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseByte(s: String): Result[NumberFormatException, Byte] =
    catching[NumberFormatException].run(s.toByte)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseDouble(s: String): Result[NumberFormatException, Double] =
    catching[NumberFormatException].run(s.toDouble)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseFloat(s: String): Result[NumberFormatException, Float] =
    catching[NumberFormatException].run(s.toFloat)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseInt(s: String): Result[NumberFormatException, Int] =
    catching[NumberFormatException].run(s.toInt)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseLong(s: String): Result[NumberFormatException, Long] =
    catching[NumberFormatException].run(s.toLong)

  /**
   * @group Create
   * @param s
   * @return
   * @since 0.2.0
   */
  def parseShort(s: String): Result[NumberFormatException, Short] =
    catching[NumberFormatException].run(s.toShort)

  /**
   * Traverses over `xs` and accumulates all results.
   *
   * @group Combine
   * @param xs a collection of values
   * @param f function to validate a single value
   * @tparam A the collection type
   * @tparam B the resulting valid type
   * @tparam E the resulting invalid type
   * @return A Result with all valid values or all invalid ones
   * @usecase def traverse[A, B, E](xs: Seq[A])(f: A => Result[E, B]): Result[E, Seq[B] ] = ??? // SI-8255
   *          @inheritdoc
   */
  def traverse[A, B, E, F[X] <: TraversableOnce[X], That](xs: F[A])(f: A => Result[E, B])(implicit cbf: CanBuildFrom[F[A], B, That]): Result[E, That] =
    xs.foldLeft(valid[E, mutable.Builder[B, That]](cbf(xs)))((acc, r) ⇒ f(r)(acc.map(b ⇒ b += _))).map(_.result())

  /**
   * Traverses over `xs` accumulating all invalids but discarding all valid values.
   *
   * @group Combine
   * @param xs a collection of values
   * @param f function to validate a single value
   * @tparam A the collection type
   * @tparam E the resulting invalid type
   * @return A Result with all invalid values or Unit
   * @since 0.2.0
   */
  def traverse_[A, E](xs: TraversableOnce[A])(f: A => Result[E, Unit]): Result[E, Unit] =
    xs.foldLeft(valid[E, Unit](()))((a, r) ⇒ f(r)(a.map(ಠ ⇒ _ ⇒ ಠ)))

  /**
   * Transforms a sequence of Results into a Result of a sequence.
   *
   * @group Combine
   * @param xs a collection of results
   * @tparam A the resulting valid type
   * @tparam E the resulting invalid type
   * @return A Result with all valid values or all invalid ones
   * @usecase def sequence[A, E](xs: Seq[Result[E, A] ]): Result[E, Seq[A] ] = ??? // SI-8255
   *          @inheritdoc
   */
  def sequence[A, E, F[X] <: TraversableOnce[X]](xs: F[Result[E, A]])(implicit cbf: CanBuildFrom[F[Result[E, A]], A, F[A]]): Result[E, F[A]] =
    traverse(xs)(x ⇒ x)

  final case class Valid[+A](value: A) extends Result[Nothing, A]
  final case class Invalid[+E](error: E) extends Result[E, Nothing]
  final case class Invalids[+E](error: NonEmptyVector[E]) extends Result[E, Nothing]

  /**
   * Curried [[Result]] type, starting with the invalid part.
   *
   * This can be used to avoid type lambdas:
   *
   * {{{
   *   // instead of
   *   type FR[+E] = Functor[({type λ[α] = Result[E, α]})#λ]
   *   // rather use
   *   type FR[+E] = Functor[Result.I[E]#V]
   * }}}
   *
   * @tparam E the resulting invalid type
   */
  sealed trait I[E] {

    type V[A] = Result[E, A]
  }

  /**
   * Curried [[Result]] type, starting with the valid part.
   *
   * This can be used to avoid type lambdas:
   *
   * {{{
   *   // instead of
   *   type FR[+A] = Functor[({type λ[α] = Result[α, A]})#λ]
   *   // rather use
   *   type FR[+A] = Functor[Result.V[A]#I]
   * }}}
   *
   * @tparam A the resulting valid type
   */
  sealed trait V[A] {

    type I[E] = Result[E, A]
  }

  final class FromTryCatchAux[T >: Null <: Throwable, R] private[validation] (leftMap: T ⇒ R) {
    /**
     * @param f
     * @param T
     * @tparam A
     * @return
     */
    def run[A](f: ⇒ A)(implicit T: ClassTag[T]): Result[R, A] =
      Exception.catching(T.runtimeClass)
        .withApply(t ⇒ invalid(leftMap(t.asInstanceOf[T])))
        .apply(valid(f))

    /**
     * @param f
     * @tparam RR
     * @return
     * @since 0.2.0
     */
    def using[RR](f: R ⇒ RR): FromTryCatchAux[T, RR] =
      new FromTryCatchAux(leftMap andThen f)
  }

  class Ap2[X, A, B](a: Result[X, A], b: Result[X, B]) {

    def apply[C](fn: (A, B) ⇒ C): Result[X, C] =
      b(a.map(fn.curried))

    def tupled: Result[X, (A, B)] =
      apply(Tuple2.apply)

    def and[C](c: Result[X, C]): Ap3[C] =
      new Ap3(c)

    sealed class Ap3[C](c: Result[X, C]) {

      def apply[D](fn: (A, B, C) ⇒ D): Result[X, D] =
        c(b(a.map(fn.curried)))

      def tupled: Result[X, (A, B, C)] =
        apply(Tuple3.apply)

      def and[D](d: Result[X, D]): Ap4[D] =
        new Ap4(d)

      class Ap4[D](d: Result[X, D]) {

        def apply[E](fn: (A, B, C, D) ⇒ E): Result[X, E] =
          d(c(b(a.map(fn.curried))))

        def tupled: Result[X, (A, B, C, D)] =
          apply(Tuple4.apply)

        def and[E](e: Result[X, E]): Ap5[E] =
          new Ap5(e)

        class Ap5[E](e: Result[X, E]) {

          def apply[F](fn: (A, B, C, D, E) ⇒ F): Result[X, F] =
            e(d(c(b(a.map(fn.curried)))))

          def tupled: Result[X, (A, B, C, D, E)] =
            apply(Tuple5.apply)

          def and[F](f: Result[X, F]): Ap6[F] =
            new Ap6(f)

          class Ap6[F](f: Result[X, F]) {

            def apply[G](fn: (A, B, C, D, E, F) ⇒ G): Result[X, G] =
              f(e(d(c(b(a.map(fn.curried))))))

            def tupled: Result[X, (A, B, C, D, E, F)] =
              apply(Tuple6.apply)

            def and[G](g: Result[X, G]): Ap7[G] =
              new Ap7(g)

            class Ap7[G](g: Result[X, G]) {

              def apply[H](fn: (A, B, C, D, E, F, G) ⇒ H): Result[X, H] =
                g(f(e(d(c(b(a.map(fn.curried)))))))

              def tupled: Result[X, (A, B, C, D, E, F, G)] =
                apply(Tuple7.apply)

              def and[H](h: Result[X, H]): Ap8[H] =
                new Ap8(h)

              class Ap8[H](h: Result[X, H]) {

                def apply[J](fn: (A, B, C, D, E, F, G, H) ⇒ J): Result[X, J] =
                  h(g(f(e(d(c(b(a.map(fn.curried))))))))

                def tupled: Result[X, (A, B, C, D, E, F, G, H)] =
                  apply(Tuple8.apply)

                def and[J](j: Result[X, J]): Ap9[J] =
                  new Ap9(j)

                class Ap9[J](i: Result[X, J]) {

                  def apply[K](fn: (A, B, C, D, E, F, G, H, J) ⇒ K): Result[X, K] =
                    i(h(g(f(e(d(c(b(a.map(fn.curried)))))))))

                  def tupled: Result[X, (A, B, C, D, E, F, G, H, J)] =
                    apply(Tuple9.apply)
                }
              }
            }
          }
        }
      }
    }
  }

  object symbolic {

    /** @since 0.2.0 */
    type \?/[+E, +A] = Result[E, A]

    implicit final class SymbolicOps[E, A](val r: E \?/ A) extends AnyVal {

      def |@|[EE >: E, AA >: A, B, C](other: EE \?/ B): Result.Ap2[EE, AA, B] =
        r.and(other)

      def unary_~ : A \?/ NonEmptyVector[E] =
        r.swap

      def ~[EE, AA](f: A \?/ NonEmptyVector[E] ⇒ AA \?/ NonEmptyVector[EE]): EE \?/ NonEmptyVector[AA] =
        r.swapped(f)

      def |[AA >: A](aa: ⇒ AA): AA =
        r.getOrElse(aa)

      def ?[AA >: A](x: NonEmptyVector[E] ⇒ AA): AA =
        r.valueOr(x)

      def |||[EE >: E, AA >: A](other: ⇒ EE \?/ AA): EE \?/ AA =
        r.orElse(other)

      def +++[EE >: E, AA >: A](other: ⇒ EE \?/ AA): EE \?/ NonEmptyVector[AA] =
        r.merge(other)

      def +|+[EE >: E, AA >: A](other: ⇒ EE \?/ AA): EE \?/ NonEmptyVector[AA] =
        r.append(other)
    }

    implicit final class Ap2SymbolicOps[X, A, B](val r: Ap2[X, A, B]) extends AnyVal {

      def |@|[C](c: X \?/ C): r.Ap3[C] =
        r.and(c)
    }

    implicit final class Ap3SymbolicOps[X, A, B, C](val r: Ap2[X, A, B]#Ap3[C]) extends AnyVal {

      def |@|[D](d: X \?/ D): r.Ap4[D] =
        r.and(d)
    }

    implicit final class Ap4SymbolicOps[X, A, B, C, D](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]) extends AnyVal {

      def |@|[E](e: X \?/ E): r.Ap5[E] =
        new r.Ap5(e)
    }

    implicit final class Ap5SymbolicOps[X, A, B, C, D, E](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]) extends AnyVal {

      def |@|[F](e: X \?/ F): r.Ap6[F] =
        new r.Ap6(e)
    }

    implicit final class Ap6SymbolicOps[X, A, B, C, D, E, F](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]) extends AnyVal {

      def |@|[G](e: X \?/ G): r.Ap7[G] =
        new r.Ap7(e)
    }

    implicit final class Ap7SymbolicOps[X, A, B, C, D, E, F, G](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]) extends AnyVal {

      def |@|[H](e: X \?/ H): r.Ap8[H] =
        new r.Ap8(e)
    }

    implicit final class Ap8SymbolicOps[X, A, B, C, D, E, F, G, H](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]#Ap8[H]) extends AnyVal {

      def |@|[J](e: X \?/ J): r.Ap9[J] =
        new r.Ap9(e)
    }
  }

  object unsafe {

    implicit final class UnsafeResultOps[E, A](val r: Result[E, A]) extends AnyVal {

      def foreach(f: A ⇒ Unit): Unit =
        r.fold(_ ⇒ (), f)

      def foreachInvalid(f: E ⇒ Unit): Unit =
        r.fold(_.toVector.foreach(f), _ ⇒ ())

      def get: A =
        r.fold(_ ⇒ sys.error("Result.invalid"), identity)

      def getInvalid: NonEmptyVector[E] =
        r.fold(identity, _ ⇒ sys.error("Result.valid"))
    }
  }

  object syntax {

    implicit final class ResultSyntax[A](val self: A) extends AnyVal {

      def valid[E]: Result[E, A] =
        Result.valid(self)

      def invalid[E]: Result[A, E] =
        Result.invalid(self)
    }
  }

  object all {
    import symbolic._
    import syntax._
    import unsafe._

    implicit def symbolicOps[E, A](r: Result[E, A]): SymbolicOps[E, A] =
      new SymbolicOps(r)

    implicit def ap2SymbolicOps[X, A, B](r: Ap2[X, A, B]): Ap2SymbolicOps[X, A, B] =
      new Ap2SymbolicOps(r)

    implicit def ap3SymbolicOps[X, A, B, C](r: Ap2[X, A, B]#Ap3[C]): Ap3SymbolicOps[X, A, B, C] =
      new Ap3SymbolicOps(r)

    implicit def ap4SymbolicOps[X, A, B, C, D](r: Ap2[X, A, B]#Ap3[C]#Ap4[D]): Ap4SymbolicOps[X, A, B, C, D] =
      new Ap4SymbolicOps(r)

    implicit def ap5SymbolicOps[X, A, B, C, D, E](r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]): Ap5SymbolicOps[X, A, B, C, D, E] =
      new Ap5SymbolicOps(r)

    implicit def ap6SymbolicOps[X, A, B, C, D, E, F](r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]): Ap6SymbolicOps[X, A, B, C, D, E, F] =
      new Ap6SymbolicOps(r)

    implicit def ap7SymbolicOps[X, A, B, C, D, E, F, G](r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]): Ap7SymbolicOps[X, A, B, C, D, E, F, G] =
      new Ap7SymbolicOps(r)

    implicit def ap8SymbolicOps[X, A, B, C, D, E, F, G, H](r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]#Ap8[H]): Ap8SymbolicOps[X, A, B, C, D, E, F, G, H] =
      new Ap8SymbolicOps(r)

    implicit def unsafeOps[E, A](r: Result[E, A]): UnsafeResultOps[E, A] =
      new UnsafeResultOps(r)

    implicit def resultSyntax[A](self: A): ResultSyntax[A] =
      new ResultSyntax(self)
  }

}
