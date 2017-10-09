/*
 * Copyright 2015 – 2017 Paul Horn
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

import org.specs2._
import org.specs2.execute._
import org.specs2.execute.Typecheck._
import org.specs2.matcher.TypecheckMatchers._

import scala.collection.immutable
import scala.util.{Failure, Success}

object ResultTranslateSpec extends Specification {
  def is = "Translating Results".title ^ s2"""

#### Result translations

$br

###### **to Option**

${Result.valid(42).toOption ==== Some(42)}
${Result.invalid(1337).toOption ==== None}

$br

###### **to immutable Seq**

${Result.valid(42).toSeq ==== immutable.Seq(42)}
${Result.invalid(1337).toSeq ==== immutable.Seq()}

$br

###### **to List**

${Result.valid(42).toList ==== List(42)}
${Result.invalid(1337).toList ==== Nil}

$br

###### **to Stream**

${Result.valid(42).toStream ==== Stream(42)}
${Result.invalid(1337).toStream ==== Stream()}

$br

###### **to Vector**

${Result.valid(42).toVector ==== Vector(42)}
${Result.invalid(1337).toVector ==== Vector()}

$br

###### **to Set**

${Result.valid(42).toSet ==== Set(42)}
${Result.invalid(1337).toSet[Int] ==== Set.empty[Int]}

$br

###### **to Either**

${Result.valid(42).toEither ==== Right(42)}
${Result.invalid(1337).toEither ==== Left(NonEmptyVector(1337))}

$br

###### **to Try**

${Result.valid(42).toTry ==== Success(42)}
${Result.invalid(Error).toTry ==== Failure(Error)}
${typecheck("Result.invalid(1337).toTry") must failWith("Cannot prove that Int <:< Throwable.")}

$br

"""

  sealed abstract class Error extends RuntimeException("error")
  case object Error extends Error
}
