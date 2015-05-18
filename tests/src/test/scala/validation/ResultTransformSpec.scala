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

import org.specs2.Specification
import validation.Result.{Invalids, Invalid, Valid}

object ResultTransformSpec extends Specification {
  def is = "Transform Results".title ^ s2"""

#### Result transformation

$br

###### **of valid values with map**

${Result.valid(1337).map(_ - 1295) ==== Valid(42)}
${Result.invalid(1337).map((_: Int) + 42) ==== Invalid(1337)}

$br

###### **of invalid values with invalidMap**

${Result.valid(42).invalidMap((_: Int) + 1295) ==== Valid(42)}
${Result.invalid(1295).invalidMap(_ + 42) ==== Invalid(1337)}

$br

###### **of both values with bimap**

${Result.valid(1337).bimap((_: Int) + 42, _ - 1295) ==== Valid(42)}
${Result.invalid(1295).bimap(_ + 42, (_: Int) + 42) ==== Invalid(1337)}

$br

###### **of valid values while discarding invalids with flatMap**

${Result.valid(1337).flatMap(a ⇒ Result.valid(a - 1295)) ==== Valid(42)}
${Result.invalid(1337).flatMap((a: Int) ⇒ Result.valid(42)) ==== Invalid(1337)}
${Result.valid(42).flatMap(a ⇒ Result.invalid(a + 1295)) ==== Invalid(1337)}
${Result.invalid(1337).flatMap((a: Int) ⇒ Result.invalid(a + 1295)) ==== Invalid(1337)}

$br

###### **of invalid values with recover**

${Result.valid(42).recover(identity) ==== Valid(42)}
${Result.invalid(1337).recover(_ - 1295) ==== Valid(42)}

$br

###### **of invalid values with recoverWith**

${Result.valid(42).recoverWith(Function.const(Result.valid(1337))) ==== Valid(42)}
${Result.invalid(1337).recoverWith(Function.const(Result.valid(42))) ==== Valid(42)}
${Result.valid(42).recoverWith(Function.const(Result.invalid(1337))) ==== Valid(42)}
${Result.invalid(42).recoverWith(Function.const(Result.invalid(1337))) ==== Invalid(1337)}

$br

###### **of swapped values**

${Result.valid(42).swapped(_.map(_ :+ 1337)) ==== Valid(NonEmptyVector.of(42))}
${Result.invalid(42).swapped(_.map(_ :+ 1337)) ==== Invalids(NonEmptyVector.of(42, 1337))}

$br

###### **swap exchanges valid and invalid**

${Result.invalid(42).swap ==== Valid(NonEmptyVector(42))}
${Result.valid(1337).swap ==== Invalid(1337)}

$br

"""

}
