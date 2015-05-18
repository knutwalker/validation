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

object ResultCombineSpec extends Specification {
  def is = "Combining Results".title ^ s2"""

#### Result combinations

$br

###### **apply the Applicative Functor**

${Result.valid(1337).apply[Int, Int](Result.valid(_ - 1295)) ==== Valid(42)}
${Result.valid(42).apply(Result.invalid(1337)) ==== Invalid(1337)}
${Result.invalid(1337).apply(Result.valid((_: Int) - 1295)) ==== Invalid(1337)}
${Result.invalid(42).apply(Result.invalid(1337)) ==== Invalids(NonEmptyVector.of(1337, 42))}

$br

###### **and accumulates invalids and combines valids**

${Result.valid[Int, Int](1337).and(Result.valid(1295)).apply(_ - _) ==== Valid(42)}
${Result.valid(42).and(Result.invalid(1337)).apply(_ - (_: Int)) ==== Invalid(1337)}
${Result.invalid(1337).and(Result.valid(1295)).apply((_: Int) - _) ==== Invalid(1337)}
${Result.invalid(42).and(Result.invalid(1337)).apply((_: Int) - (_: Int)) ==== Invalids(NonEmptyVector.of(42, 1337))}

$br

###### **zip accumulates invalids and tuples valids**

${Result.valid[Int, Int](42).zip(Result.valid(42)) ==== Valid((42, 42))}
${Result.valid(42).zip(Result.invalid(1337)) ==== Invalid(1337)}
${Result.invalid(1337).zip(Result.valid(42)) ==== Invalid(1337)}
${Result.invalid(42).zip(Result.invalid(1337)) ==== Invalids(NonEmptyVector.of(42, 1337))}

$br

###### **orElse is a alternative**

${Result.valid(42).orElse(Result.valid(1337)) ==== Valid(42)}
${Result.valid(42).orElse(Result.invalid(1337)) ==== Valid(42)}
${Result.invalid(1337).orElse(Result.valid(42)) ==== Valid(42)}
${Result.invalid(42).orElse(Result.invalid(1337)) ==== Invalid(1337)}

$br

###### **merge accumulates either two valids or invalids and otherwise returns the invalid**

${Result.valid[Int, Int](1337).merge(Result.valid(42)) ==== Valid(NonEmptyVector.of(1337, 42))}
${Result.valid(42).merge(Result.invalid(1337)) ==== Invalid(1337)}
${Result.invalid(1337).merge(Result.valid(42)) ==== Invalid(1337)}
${Result.invalid[Int, Int](42).merge(Result.invalid(1337)) ==== Invalids(NonEmptyVector.of(1337, 42))}

$br

###### **append accumulates either two valids or invalids and otherwise returns the valid**

${Result.valid[Int, Int](1337).append(Result.valid(42)) ==== Valid(NonEmptyVector.of(1337, 42))}
${Result.valid(42).append(Result.invalid(1337)) ==== Valid(NonEmptyVector.of(42))}
${Result.invalid(1337).append(Result.valid(42)) ==== Valid(NonEmptyVector.of(42))}
${Result.invalid[Int, Int](42).append(Result.invalid(1337)) ==== Invalids(NonEmptyVector.of(1337, 42))}

$br

"""

}
