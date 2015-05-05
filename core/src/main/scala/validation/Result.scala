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


sealed trait Result[+E, +A] extends Any with Product with Serializable {

  /**
   * Run the first function if the Result is invalid,
   * otherwise, run the second function.
   * This is the Catamorphism.
   */
  def fold[B](fe: E ⇒ B, fa: A ⇒ B): B = this match {
    case Result.Invalid(e) ⇒ fe(e)
    case Result.Valid(a)   ⇒ fa(a)
  }

  def invalid[EE >: E, AA >: A, B](fe: EE ⇒ B): Result.InvalidThenValid[EE, AA, B] =
    new Result.InvalidThenValid(this, fe)

  def valid[EE >: E, AA >: A, B](fa: AA ⇒ B): Result.ValidThenInvalid[EE, AA, B] =
    new Result.ValidThenInvalid(this, fa)

  /**
   * flatMap over the Result if it is valid.
   * flatMap does not accumulate errors. If you want to do so,
   * use [[and]] instead.
   */
  def flatMap[EE >: E, B](f: A ⇒ Result[EE, B]): Result[EE, B] =
    fold(Result.invalid, f)

  def bimap[EE, AA](fe: E ⇒ EE, fa: A ⇒ AA): Result[EE, AA] =
    fold(fe andThen Result.invalid, fa andThen Result.valid)

  def map[B](f: A ⇒ B): Result[E, B] =
    bimap(identity, f)

  def invalidMap[F](f: E ⇒ F): Result[F, A] =
    bimap(f, identity)

  def recover[AA >: A](f: E ⇒ AA): Result[E, AA] =
    fold(f andThen Result.valid, _ ⇒ this)

  def apply[EE >: E, B](f: Result[EE, A ⇒ B])(implicit EE: Mergeable[EE]): Result[EE, B] =
    fold(
      e1 ⇒ f.fold(e2 ⇒ Result.invalid(EE.merge(e1, e2)), _ ⇒ Result.invalid(e1)),
      a1 ⇒ f.fold(Result.invalid, ab ⇒ Result.valid(ab(a1)))
    )

  def filter[EE >: E](p: A ⇒ Boolean, ifEmpty: ⇒ EE): Result[EE, A] =
    fold(_ ⇒ this, a ⇒ if (p(a)) this else Result.invalid(ifEmpty))

  def and[EE >: E, AA >: A, B, C](other: Result[EE, B])(implicit EE: Mergeable[EE]): Result.Ap2[EE, AA, B] =
    new Result.Ap2(this, other)

  def zip[EE >: E, B](b: Result[EE, B])(implicit EE: Mergeable[EE]): Result[EE, (A, B)] =
    and(b).tupled

  def foldLeft[B](b: B)(f: (B, A) ⇒ B): B =
    fold(_ ⇒ b, f(b, _))

  def foldRight[B](b: B)(f: (A, ⇒ B) => B): B =
    fold(_ ⇒ b, f(_, b))

  def isValid: Boolean =
    fold(_ ⇒ false, _ ⇒ true)

  def isInvalid: Boolean =
    !isValid

  def exists(p: A ⇒ Boolean): Boolean =
    fold(_ ⇒ false, p)

  def forall(p: A ⇒ Boolean): Boolean =
    fold(_ ⇒ true, p)

  def toOption: Option[A] =
    fold(_ ⇒ None, Some.apply)

  def toSeq: immutable.Seq[A] =
    fold(_ ⇒ immutable.Seq.empty, immutable.Seq(_))

  def toList: List[A] =
    fold(_ ⇒ Nil, List(_))

  def toStream: Stream[A] =
    fold(_ ⇒ Stream.empty, Stream(_))

  def toVector: Vector[A] =
    fold(_ ⇒ Vector.empty, Vector(_))

  def toSet[AA >: A]: Set[AA] =
    fold(_ ⇒ Set.empty, Set(_))

  def toEither: Either[E, A] =
    fold(Left.apply, Right.apply)

  def toTry(implicit ev: E <:< Throwable): Try[A] =
    fold(ev andThen Failure.apply, Success.apply)

  def swap: Result[A, E] =
    fold(Result.valid, Result.invalid)

  def swapped[EE, AA](f: Result[A, E] ⇒ Result[AA, EE]): Result[EE, AA] =
    f(swap).swap

  def getOrElse[AA >: A](aa: ⇒ AA): AA =
    fold(_ ⇒ aa, identity)

  def valueOr[AA >: A](x: E ⇒ AA): AA =
    fold(x, identity)

  def orElse[EE >: E, AA >: A](other: ⇒ Result[EE, AA]): Result[EE, AA] =
    fold(_ ⇒ other, _ ⇒ this)

  def merge[EE >: E, AA >: A](other: ⇒ Result[EE, AA])(implicit EE: Mergeable[EE], AA: Mergeable[AA]): Result[EE, AA] =
    fold(
      e1 ⇒ other.fold(e2 ⇒ Result.invalid(EE.merge(e1, e2)), _ ⇒ this),
      a1 ⇒ other.fold(_ ⇒ other, a2 ⇒ Result.valid(AA.merge(a1, a2)))
    )

  def append[EE >: E, AA >: A](other: ⇒ Result[EE, AA])(implicit EE: Mergeable[EE], AA: Mergeable[AA]): Result[EE, AA] =
    fold(
      e1 ⇒ other.fold(e2 ⇒ Result.invalid(EE.merge(e1, e2)), _ ⇒ other),
      a1 ⇒ other.fold(_ ⇒ this, a2 ⇒ Result.valid(AA.merge(a1, a2)))
    )

  def compare[EE >: E, AA >: A](other: Result[EE, AA])(implicit EE: Ordering[EE], AA: Ordering[AA]): Int =
    fold(
      e ⇒ other.fold(EE.compare(e, _), _ ⇒ -1),
      a ⇒ other.fold(_ ⇒ 1, AA.compare(a, _))
    )

  def ==[EE >: E, AA >: A](other: Result[EE, AA])(implicit EE: Equiv[EE], AA: Equiv[AA]): Boolean =
    fold(
      e ⇒ other.fold(EE.equiv(e, _), _ ⇒ false),
      a ⇒ other.fold(_ ⇒ false, AA.equiv(a, _))
    )
}
object Result {

  def valid[E, A](x: A): Result[E, A] =
    new Valid[A](x)

  def invalid[E, A](e: E): Result[E, A] =
    new Invalid[E](e)

  def traverse[A, B, E, F[X] <: TraversableOnce[X], That](xs: F[A])(f: A => Result[E, B])(implicit E: Mergeable[E], cbf: CanBuildFrom[F[A], B, That]): Result[E, That] =
    xs.foldLeft(valid[E, mutable.Builder[B, That]](cbf(xs)))((acc, r) ⇒ f(r)(acc.map(b ⇒ b += _))).map(_.result())

  def sequence[A, E, F[X] <: TraversableOnce[X]](xs: F[Result[E, A]])(implicit E: Mergeable[E], cbf: CanBuildFrom[F[Result[E, A]], A, F[A]]): Result[E, F[A]] =
    traverse(xs)(x ⇒ x)

  def fromTry[A](x: Try[A]): Result[Throwable, A] = x match {
    case Failure(e) ⇒ invalid(e)
    case Success(a) ⇒ valid(a)
  }

  def fromEither[E, A](x: Either[E, A]): Result[E, A] = x match {
    case Left(e)  ⇒ invalid(e)
    case Right(a) ⇒ valid(a)
  }

  def fromOption[E, A](x: Option[A], ifNone: ⇒ E): Result[E, A] = x match {
    case None    ⇒ invalid(ifNone)
    case Some(a) ⇒ valid(a)
  }

  def catching[T >: Null <: Throwable : ClassTag]: FromTryCatchAux[T] =
    new FromTryCatchAux[T]

  sealed case class Valid[+A](value: A) extends Result[Nothing, A]
  sealed case class Invalid[+E](error: E) extends Result[E, Nothing]

  trait I[E] {

    type V[A] = Result[E, A]
  }

  trait V[A] {

    type I[E] = Result[E, A]
  }

  final class InvalidThenValid[E, A, X] private[validation](r: Result[E, A], fe: E ⇒ X) {
    def valid(fa: A ⇒ X): X =
      r.fold(fe, fa)
  }

  final class ValidThenInvalid[E, A, X] private[validation](r: Result[E, A], fa: A ⇒ X) {
    def invalid(fe: E ⇒ X): X =
      r.fold(fe, fa)
  }

  final class FromTryCatchAux[T] private[validation] {
    def run[A](f: ⇒ A)(implicit T: ClassTag[T]): Result[T, A] =
      Exception.catching(T.runtimeClass).withApply(t ⇒ invalid(t.asInstanceOf[T]))(valid(f))
  }

  class Ap2[X, A, B](a: Result[X, A], b: Result[X, B]) {

    def apply[C](fn: (A, B) ⇒ C)(implicit X: Mergeable[X]): Result[X, C] =
      b(a.map(fn.curried))

    def tupled(implicit X: Mergeable[X]): Result[X, (A, B)] =
      apply(Tuple2.apply)

    def and[C](c: Result[X, C]): Ap3[C] =
      new Ap3(c)

    sealed class Ap3[C](c: Result[X, C]) {

      def apply[D](fn: (A, B, C) ⇒ D)(implicit X: Mergeable[X]): Result[X, D] =
        c(b(a.map(fn.curried)))

      def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C)] =
        apply(Tuple3.apply)

      def and[D](d: Result[X, D]): Ap4[D] =
        new Ap4(d)

      class Ap4[D](d: Result[X, D]) {

        def apply[E](fn: (A, B, C, D) ⇒ E)(implicit X: Mergeable[X]): Result[X, E] =
          d(c(b(a.map(fn.curried))))

        def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D)] =
          apply(Tuple4.apply)

        class Ap5[E](e: Result[X, E]) {

          def apply[F](fn: (A, B, C, D, E) ⇒ F)(implicit X: Mergeable[X]): Result[X, F] =
            e(d(c(b(a.map(fn.curried)))))

          def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D, E)] =
            apply(Tuple5.apply)

          class Ap6[F](f: Result[X, F]) {

            def apply[G](fn: (A, B, C, D, E, F) ⇒ G)(implicit X: Mergeable[X]): Result[X, G] =
              f(e(d(c(b(a.map(fn.curried))))))

            def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D, E, F)] =
              apply(Tuple6.apply)

            class Ap7[G](g: Result[X, G]) {

              def apply[H](fn: (A, B, C, D, E, F, G) ⇒ H)(implicit X: Mergeable[X]): Result[X, H] =
                g(f(e(d(c(b(a.map(fn.curried)))))))

              def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D, E, F, G)] =
                apply(Tuple7.apply)

              class Ap8[H](h: Result[X, H]) {

                def apply[J](fn: (A, B, C, D, E, F, G, H) ⇒ J)(implicit X: Mergeable[X]): Result[X, J] =
                  h(g(f(e(d(c(b(a.map(fn.curried))))))))

                def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D, E, F, G, H)] =
                  apply(Tuple8.apply)

                class Ap9[J](i: Result[X, J]) {

                  def apply[K](fn: (A, B, C, D, E, F, G, H, J) ⇒ K)(implicit X: Mergeable[X]): Result[X, K] =
                    i(h(g(f(e(d(c(b(a.map(fn.curried)))))))))

                  def tupled(implicit X: Mergeable[X]): Result[X, (A, B, C, D, E, F, G, H, J)] =
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

    implicit final class SymbolicOps[E, A](val r: Result[E, A]) extends AnyVal {

      def |@|[EE >: E, AA >: A, B, C](other: Result[EE, B])(implicit EE: Mergeable[EE]): Result.Ap2[EE, AA, B] =
        r.and(other)

      def unary_~ : Result[A, E] =
        r.swap

      def ~[EE, AA](f: Result[A, E] ⇒ Result[AA, EE]): Result[EE, AA] =
        r.swapped(f)

      def |[AA >: A](aa: ⇒ AA): AA =
        r.getOrElse(aa)

      def ?[AA >: A](x: E ⇒ AA): AA =
        r.fold(x, identity)

      def |||[EE >: E, AA >: A](other: ⇒ Result[EE, AA]): Result[EE, AA] =
        r.orElse(other)

      def +++[EE >: E, AA >: A](other: ⇒ Result[EE, AA])(implicit EE: Mergeable[EE], AA: Mergeable[AA]): Result[EE, AA] =
        r.merge(other)

      def +|+[EE >: E, AA >: A](other: ⇒ Result[EE, AA])(implicit EE: Mergeable[EE], AA: Mergeable[AA]): Result[EE, AA] =
        r.append(other)
    }

    implicit final class Ap2SymbolicOps[X, A, B](val r: Ap2[X, A, B]) extends AnyVal {

      def |@|[C](c: Result[X, C]): r.Ap3[C] =
        r.and(c)
    }

    implicit final class Ap3SymbolicOps[X, A, B, C](val r: Ap2[X, A, B]#Ap3[C]) extends AnyVal {

      def |@|[D](d: Result[X, D]): r.Ap4[D] =
        r.and(d)
    }

    implicit final class Ap4SymbolicOps[X, A, B, C, D](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]) extends AnyVal {

      def |@|[E](e: Result[X, E]): r.Ap5[E] =
        new r.Ap5(e)
    }

    implicit final class Ap5SymbolicOps[X, A, B, C, D, E](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]) extends AnyVal {

      def |@|[F](e: Result[X, F]): r.Ap6[F] =
        new r.Ap6(e)
    }

    implicit final class Ap6SymbolicOps[X, A, B, C, D, E, F](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]) extends AnyVal {

      def |@|[G](e: Result[X, G]): r.Ap7[G] =
        new r.Ap7(e)
    }

    implicit final class Ap7SymbolicOps[X, A, B, C, D, E, F, G](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]) extends AnyVal {

      def |@|[H](e: Result[X, H]): r.Ap8[H] =
        new r.Ap8(e)
    }

    implicit final class Ap8SymbolicOps[X, A, B, C, D, E, F, G, H](val r: Ap2[X, A, B]#Ap3[C]#Ap4[D]#Ap5[E]#Ap6[F]#Ap7[G]#Ap8[H]) extends AnyVal {

      def |@|[J](e: Result[X, J]): r.Ap9[J] =
        new r.Ap9(e)
    }
  }

  object unsafe {

    implicit final class UnsafeOps[E, A](val r: Result[E, A]) extends AnyVal {

      def foreach(f: A ⇒ Unit): Unit =
        r.fold(_ ⇒ (), f)

      def get: A =
        r.fold(_ ⇒ sys.error("Result.invalid"), identity)

      def getInvalid: E =
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

    implicit def unsafeOps[E, A](r: Result[E, A]): UnsafeOps[E, A] =
      new UnsafeOps(r)

    implicit def resultSyntax[A](self: A): ResultSyntax[A] =
      new ResultSyntax(self)
  }

}
