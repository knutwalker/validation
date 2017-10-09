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

import org.specs2.Specification
import validation.Result.{Invalid, Valid}

import scala.util.Try

object ResultCreationSpec extends Specification {
  def is = "Creating Results".title ^ s2"""

#### Create a new Result

$br

###### **by using smart constructor methods**

${Result.valid(42) ==== Valid(42)}
${Result.invalid(1337) ==== Invalid(1337)}

$br

###### **from a Try**

${Result.fromTry(Try(42)) ==== Valid(42)}
${Result.fromTry(Try(throw Error)) ==== Invalid(Error)}

$br

###### **from an Either**

${Result.fromEither(Right(42)) ==== Valid(42)}
${Result.fromEither(Left(1337)) ==== Invalid(1337)}

$br

###### **from an Option**

${Result.fromOption(Some(42), 1337) ==== Valid(42)}
${Result.fromOption(None, 1337) ==== Invalid(1337)}

$br

###### **by catching a specific exception**

${Result.catching[Error].run(42) ==== Valid(42)}
${Result.catching[Error].run(throw Error) ==== Invalid(Error)}
${Result.catching[Error].using(_.getMessage).run(throw Error) ==== Invalid("error")}

$br

"""

  sealed abstract class Error extends RuntimeException("error")
  case object Error extends Error
}
