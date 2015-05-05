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

import concurrent.{ExecutionContext, Future}
import scala.annotation.implicitNotFound
import scala.collection.immutable

import java.math.BigInteger

/**
 * Semigroup
 */
@implicitNotFound("Don't know how to merge two ${A}s.")
sealed trait Mergeable[A] extends Any with Serializable {

  def merge(a: A, b: ⇒ A): A
}
object Mergeable extends MergeableFunctions with MergeableInstances {
  @inline def apply[A](implicit A: Mergeable[A]): Mergeable[A] = A
}

trait MergeableFunctions {

  def mergeable[A](f: (A, ⇒ A) ⇒ A): Mergeable[A] =
    new Mergeable[A] {
      def merge(a: A, b: ⇒ A): A = f(a, b)
    }

  def instance[A](f: (A, A) ⇒ A): Mergeable[A] =
    new Mergeable[A] {
      def merge(a: A, b: ⇒ A): A = f(a, b)
    }

  def firstMergebale[A]: Mergeable[A] =
    new Mergeable[A] {
      def merge(a: A, b: ⇒ A): A = a
    }

  def lastMergeable[A]: Mergeable[A] =
    new Mergeable[A] {
      def merge(a: A, b: ⇒ A): A = b
    }

  def minMergeable[A](implicit A: Ordering[A]): Mergeable[A] =
    instance(A.min)

  def maxMergeable[A](implicit A: Ordering[A]): Mergeable[A] =
    instance(A.max)
}

trait MergeableInstances extends LowPriorityInstances {this: MergeableFunctions ⇒

  implicit val mergeableUnit: Mergeable[Unit] =
    mergeable((_, _) ⇒ ())

  implicit val mergeableByte: Mergeable[Byte] =
    instance((b1, b2) ⇒ (b1 + b2).toByte)

  implicit val mergeableChar: Mergeable[Char] =
    instance((c1, c2) ⇒ (c1 + c2).toChar)

  implicit val mergeableShort: Mergeable[Short] =
    instance((s1, s2) ⇒ (s1 + s2).toShort)

  implicit val mergeableInt: Mergeable[Int] =
    instance(_ + _)

  implicit val mergeableLong: Mergeable[Long] =
    instance(_ + _)

  implicit val mergeableFloat: Mergeable[Float] =
    instance(_ + _)

  implicit val mergeableDouble: Mergeable[Double] =
    instance(_ + _)

  implicit val mergeableBigInt: Mergeable[BigInt] =
    instance(_ + _)

  implicit val mergeableBigInteger: Mergeable[BigInteger] =
    instance(_ add _)

  implicit val mergeableBigDecimal: Mergeable[BigDecimal] =
    instance(_ + _)

  implicit val mergeableString: Mergeable[String] =
    instance(_ + _)

  implicit def mergeableVector[A]: Mergeable[Vector[A]] =
    instance(_ ++ _)

  implicit def mergeableList[A]: Mergeable[List[A]] =
    instance(_ ++ _)

  implicit def mergeableSet[A]: Mergeable[Set[A]] =
    instance(_ | _)

  implicit def mergeableStream[A]: Mergeable[Stream[A]] =
    instance(_ ++ _)

  implicit def mergeableSeq[A]: Mergeable[immutable.Seq[A]] =
    instance(_ ++ _)

  implicit def mergeableIterable[A]: Mergeable[Iterable[A]] =
    instance(_ ++ _)

  implicit def mergeableMap[K, V]: Mergeable[Map[K, V]] =
    instance(_ ++ _)

  implicit val mergeableStringBuilder: Mergeable[StringBuilder] =
    instance(_ append _)

  implicit val mergeableStringBuffer: Mergeable[StringBuffer] =
    instance(_ append _)

  implicit def mergeableFunction1[A, R](implicit R: Mergeable[R]): Mergeable[A ⇒ R] =
    mergeable((f1, f2) ⇒ a ⇒ R.merge(f1(a), f2.apply(a)))

  implicit def mergeableFuture[A](implicit A: Mergeable[A], ec: ExecutionContext): Mergeable[Future[A]] =
    instance((f1, f2) ⇒ for (a ← f1; b ← f1) yield A.merge(a, b))

  implicit def mergeableTuple1[A](implicit A: Mergeable[A]): Mergeable[Tuple1[A]] =
    mergeable((t1, t2) ⇒ Tuple1(A.merge(t1._1, t2._1)))

  implicit def mergeableTuple2[A, B](implicit A: Mergeable[A], B: Mergeable[B]): Mergeable[(A, B)] =
    mergeable((t1, t2) ⇒ {
      lazy val _t2 = t2
      (A.merge(t1._1, _t2._1), B.merge(t1._2, _t2._2))
    })

  implicit def mergeableTuple3[A, B, C](implicit A: Mergeable[A], B: Mergeable[B], C: Mergeable[C]): Mergeable[(A, B, C)] =
    mergeable((t1, t2) ⇒ {
      lazy val _t2 = t2
      (A.merge(t1._1, _t2._1), B.merge(t1._2, _t2._2), C.merge(t1._3, t2._3))
    })

  implicit def mergeableTuple4[A, B, C, D](implicit A: Mergeable[A], B: Mergeable[B], C: Mergeable[C], D: Mergeable[D]): Mergeable[(A, B, C, D)] =
    mergeable((t1, t2) ⇒ {
      lazy val _t2 = t2
      (A.merge(t1._1, _t2._1), B.merge(t1._2, _t2._2), C.merge(t1._3, t2._3), D.merge(t1._4, t2._4))
    })


  val mergeableDisjunctionBoolean: Mergeable[Boolean] =
    mergeable(_ || _)

  val mergeableExclusiveDisjunctionBoolean: Mergeable[Boolean] =
    instance((p, q) ⇒ p && !q || !p && q)

  def mergeableLastOption[A]: Mergeable[Option[A]] =
    instance((o1, o2) ⇒ o2 orElse o1)

}
trait LowPriorityInstances {this: MergeableFunctions ⇒

  implicit val mergeableConjunctionBoolean: Mergeable[Boolean] =
    mergeable(_ && _)

  implicit def mergeableFirstOption[A]: Mergeable[Option[A]] =
    mergeable(_ orElse _)

}
