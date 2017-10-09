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
import validation.Result.{Valid, Invalid}

object ResultTestSpec extends Specification {
  def is = "Testing Results".title ^ s2"""

#### Result value testing

$br

###### **filtering with filter**

${Result.valid(42).filter(_ == 42, 1337) ==== Valid(42)}
${Result.valid(42).filter(_ != 42, 1337) ==== Invalid(1337)}
${Result.invalid(1337).filter(_ == 42, 42) ==== Invalid(1337)}

$br

###### **checking for validity**

${Result.valid(42).isValid ==== true}
${Result.invalid(1337).isValid ==== false}
${Result.valid(42).isInvalid ==== false}
${Result.invalid(1337).isInvalid ==== true}

$br

###### **exists tests if a Result is valid and a predicate holds**

${Result.valid(42).exists(_ == 42) ==== true}
${Result.valid(42).exists(_ == 1337) ==== false}
${Result.invalid(42).exists(_ == 42) ==== false}
${Result.invalid(42).exists(_ == 1337) ==== false}

$br

###### **forall tests if a Result is either invalid or satisfies a predicate**

${Result.valid(42).forall(_ == 42) ==== true}
${Result.valid(42).forall(_ == 1337) ==== false}
${Result.invalid(42).forall(_ == 42) ==== true}
${Result.invalid(42).forall(_ == 1337) ==== true}

$br

###### **contains tests if a Result has a specific valid value**

${Result.valid(42).contains(42) ==== true}
${Result.valid(42).contains(1337) ==== false}
${Result.invalid(42).contains(42) ==== false}
${Result.invalid(42).contains(1337) ==== false}

$br

"""

}
