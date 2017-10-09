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

object ResultMiscSpec extends Specification {
  def is = "Miscellaneous Results".title ^ s2"""

#### Miscellaneous operations on Result

$br

###### **compare compares two results based on the Ordering typeclass**

${Result.valid(42).compare[Int, Int](Result.valid(42)) ==== 0}
${Result.valid(42).compare[Int, Int](Result.valid(0)) ==== 1}
${Result.valid(42).compare[Int, Int](Result.valid(1337)) ==== -1}
${Result.valid(42).compare(Result.invalid(42)) ==== 1}
${Result.valid(42).compare(Result.invalid(0)) ==== 1}
${Result.valid(42).compare(Result.invalid(1337)) ==== 1}
${Result.invalid(1337).compare(Result.valid(42)) ==== -1}
${Result.invalid(1337).compare(Result.valid(0)) ==== -1}
${Result.invalid(1337).compare(Result.valid(1337)) ==== -1}
${Result.invalid(1337).compare[Int, Int](Result.invalid(1337)) ==== 0}
${Result.invalid(1337).compare[Int, Int](Result.invalid(42)) ==== 1}
${Result.invalid(1337).compare[Int, Int](Result.invalid(133742)) ==== -1}

$br

###### **== compares two results based on the Equiv typeclass**

${Result.valid(42) == Result.valid(42) ==== true}
${Result.valid(42) == Result.valid(1337) ==== false}
${Result.valid(42) == Result.invalid(42) ==== false}
${Result.valid(42) == Result.invalid(1337) ==== false}
${Result.invalid(1337) == Result.valid(42) ==== false}
${Result.invalid(1337) == Result.valid(1337) ==== false}
${Result.invalid(1337) == Result.invalid(1337) ==== true}
${Result.invalid(1337) == Result.invalid(42) ==== false}

$br

"""

}
