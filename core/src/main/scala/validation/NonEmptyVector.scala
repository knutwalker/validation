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

import scala.collection.immutable.Vector
import scala.collection.immutable.VectorBuilder

/**
 * A Vector that is guaranteed to be non-empty.
 */
final class NonEmptyVector[+A] private (val head: A, val tail: Vector[A]) extends Serializable {

  def isEmpty: Boolean =
    false

  def nonEmpty: Boolean =
    true

  def forall(p: A ⇒ Boolean): Boolean =
    p(head) && tail.forall(p)

  def exists(p: (A) ⇒ Boolean): Boolean =
    p(head) || tail.exists(p)

  def find(p: (A) ⇒ Boolean): Option[A] =
    if (p(head)) Some(head) else tail.find(p)

  def last: A =
    tail.lastOption.getOrElse(head)

  def tailOption: Option[NonEmptyVector[A]] =
    if (tail.isEmpty) None else Some(NonEmptyVector(tail.head, tail.tail))

  def init: Vector[A] =
    if (tail.isEmpty) Vector.empty[A]
    else head +: tail.init

  def +:[AA >: A](x: AA): NonEmptyVector[AA] =
    NonEmptyVector(x, toVector)

  def :+[AA >: A](x: AA): NonEmptyVector[AA] =
    NonEmptyVector(head, tail :+ x)

  def ++[AA >: A](xs: NonEmptyVector[AA]): NonEmptyVector[AA] =
    NonEmptyVector(head, (tail :+ xs.head) ++ xs.tail)

  def map[B](f: A ⇒ B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail map f)

  def flatMap[B](f: A ⇒ NonEmptyVector[B]): NonEmptyVector[B] = {
    val b = new VectorBuilder[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach { a ⇒
      val p = f(a)
      b += p.head
      b ++= p.tail
    }
    val bb = b.result()
    NonEmptyVector(bb.head, bb.tail)
  }

  def filter(f: A ⇒ Boolean): Vector[A] =
    toVector.filter(f)

  def filterNot(f: A ⇒ Boolean): Vector[A] =
    toVector.filterNot(f)

  def foldLeft[B](z: B)(f: (B, A) ⇒ B): B =
    toVector.foldLeft(z)(f)

  def reduceLeft[B >: A](f: (B, A) ⇒ B): B =
    tail.foldLeft(head: B)(f)

  def reduce[B](f: A ⇒ B)(g: (B, ⇒ B) ⇒ B): B =
    tail.foldLeft(f(head))((b, a) ⇒ g(b, f(a)))

  def distinct: NonEmptyVector[A] =
    NonEmptyVector.unsafeApply(toVector.distinct)

  def tails: NonEmptyVector[NonEmptyVector[A]] =
    NonEmptyVector(this, if (tail.isEmpty) Vector() else NonEmptyVector.unsafeApply(tail).tails.toVector)

  def tailz: Stream[NonEmptyVector[A]] =
    this #:: (if (tail.isEmpty) Stream() else NonEmptyVector.unsafeApply(tail).tailz)

  def reverse: NonEmptyVector[A] =
    NonEmptyVector.unsafeApply(toVector.reverse)

  def sortBy[B](f: A ⇒ B)(implicit B: Ordering[B]): NonEmptyVector[A] =
    NonEmptyVector.unsafeApply(toVector.sortBy(f))

  def sortWith(lt: (A, A) ⇒ Boolean): NonEmptyVector[A] =
    NonEmptyVector.unsafeApply(toVector.sortWith(lt))

  def sorted[B >: A](implicit B: Ordering[B]): NonEmptyVector[A] =
    NonEmptyVector.unsafeApply(toVector.sorted[B])

  def size: Int =
    1 + tail.size

  def zip[B](b: NonEmptyVector[B]): NonEmptyVector[(A, B)] =
    NonEmptyVector((head, b.head), tail zip b.tail)

  def unzip[X, Y](implicit ev: A <:< (X, Y)): (NonEmptyVector[X], NonEmptyVector[Y]) = {
    val (x, y) = ev(head)
    val (xx, yy) = tail.unzip
    (NonEmptyVector(x, xx), NonEmptyVector(y, yy))
  }

  def toVector: Vector[A] =
    head +: tail

  def toList: List[A] =
    head :: tail.toList

  def toStream: Stream[A] =
    head #:: tail.toStream

  def compare[AA >: A](other: NonEmptyVector[AA])(implicit AA: Ordering[AA]): Int =
    Ordering.Iterable[AA].compare(toVector, other.toVector)

  def ==[AA >: A](other: NonEmptyVector[AA])(implicit AA: Equiv[AA]): Boolean =
    compare(other)(Ordering.fromLessThan((a, b) ⇒ !AA.equiv(a, b))) == 0

  override def toString: String =
    toVector.mkString("NonEmptyVector(", ",", ")")

  override def equals(any: Any): Boolean = any match {
    case other: NonEmptyVector[_] ⇒ toVector == other.toVector
    case otherwise                ⇒ false
  }

  override def hashCode: Int =
    toVector.hashCode()
}
object NonEmptyVector {

  def apply[A](single: A): NonEmptyVector[A] =
    apply[A](single, Vector.empty[A])

  def apply[A](head: A, tail: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector[A](head, tail)

  def unapply[A](x: NonEmptyVector[A]): Option[(A, Vector[A])] =
    Some((x.head, x.tail))

  def unapply[A](x: Vector[A]): Option[(NonEmptyVector[A])] =
    NonEmptyVector.fromVector(x)

  def of[A](a: A, as: A*): NonEmptyVector[A] =
    apply[A](a, as.toVector)

  def fromVector[A](xs: Vector[A]): Option[NonEmptyVector[A]] =
    xs.headOption.map(x ⇒ apply[A](x, xs.tail))

  def fromList[A](xs: List[A]): Option[NonEmptyVector[A]] =
    xs.headOption.map(x ⇒ apply[A](x, xs.tail.toVector))

  private def unsafeApply[A](xs: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector[A](xs.head, xs.tail)

  object syntax {

    implicit final class AnyToNevSyntax[A](val self: A) extends AnyVal {

      def wrapNev: NonEmptyVector[A] =
        NonEmptyVector[A](self)
    }

    implicit final class ListToNevSyntax[A](val self: List[A]) extends AnyVal {

      def toNev: Option[NonEmptyVector[A]] =
        NonEmptyVector.fromList[A](self)
    }

    implicit final class VectorToNevSyntax[A](val self: Vector[A]) extends AnyVal {

      def toNev: Option[NonEmptyVector[A]] =
        NonEmptyVector.fromVector[A](self)
    }
  }

  object unsafe {

    implicit final class UnsafeNonEmptyVectorOps[A](val nev: NonEmptyVector[A]) extends AnyVal {

      def foreach(f: A ⇒ Unit): Unit = {
        f(nev.head)
        nev.tail.foreach(f)
      }
    }
  }
}
