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

object ResultAccessSpec extends Specification {
  def is = "Accessing Results".title ^ s2"""

#### Get a value from Result

$br

###### **with getOrElse**

${Result.valid(42).getOrElse(1337) ==== 42}
${Result.invalid(42).getOrElse(1337) ==== 1337}

$br

###### **with valueOr**

${Result.valid(42).valueOr(identity) ==== 42}
${Result.invalid(1337).valueOr(identity) ==== 1337}

$br

###### **with fold**

${Result.valid(42).fold(identity, identity) ==== 42}
${Result.invalid(1337).fold(identity, identity) ==== 1337}

$br

###### **with valid/invalid (curried fold)**

${Result.valid(42).valid(identity)(identity) ==== 42}
${Result.invalid(1337).valid(identity)(identity) ==== 1337}

$br

###### **with invalid/valid (curried fold)**

${Result.valid(42).invalid(identity)(identity) ==== 42}
${Result.invalid(1337).invalid(identity)(identity) ==== 1337}

$br

###### **with foldLeft**

${Result.valid(42).foldLeft(1337)((b, a) ⇒ a) ==== 42}
${Result.invalid(1337).foldLeft(1337)((b, a) ⇒ a) ==== 1337}

$br

###### **with foldRight**

${Result.valid(42).foldRight(1337)((a, b) ⇒ a) ==== 42}
${Result.invalid(1337).foldRight(1337)((a, b) ⇒ a) ==== 1337}

$br

"""

}
