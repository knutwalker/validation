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
import org.specs2.execute._
import org.specs2.execute.Typecheck._
import org.specs2.matcher.TypecheckMatchers._


object ResultUnsafeSpec extends Specification {
  def is = "Unsafe Operations on Results".title ^ s2"""

#### Unsafe Operations on Results

$br

###### **get is an unsafe operation**

${typecheck("Result.valid(42).get") must failWith("value get is not a member of validation.Result")}
${typecheck("Result.invalid(1337).get") must failWith("value get is not a member of validation.Result")}
${import Result.unsafe._; Result.valid(42).get ==== 42}
${import Result.unsafe._; Result.invalid(1337).get must throwA[RuntimeException](message = "Result.invalid")}

$br

###### **getInvalid is an unsafe operation**

${typecheck("Result.valid(42).getInvalid") must failWith("value getInvalid is not a member of validation.Result")}
${typecheck("Result.invalid(1337).getInvalid") must failWith("value getInvalid is not a member of validation.Result")}
${import Result.unsafe._; Result.valid(42).getInvalid must throwA[RuntimeException](message = "Result.valid")}
${import Result.unsafe._; Result.invalid(1337).getInvalid ==== 1337}

$br

###### **foreach is an unsafe operation**

${typecheck("Result.valid(42).foreach(_ ==== 42)") must failWith("value foreach is not a member of validation.Result")}
${typecheck("Result.invalid(1337).foreach(_ ==== 1337)") must failWith("value foreach is not a member of validation.Result")}
${import Result.unsafe._; Result.valid(42).foreach(_ ==== 42); true}
${import Result.unsafe._; Result.invalid(1337).foreach((_: Int) ⇒ failure); true}

$br

"""

}
