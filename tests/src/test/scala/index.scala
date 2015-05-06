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

import org.specs2.Specification
import validation.{ResultUnsafeSpec, ResultMiscSpec, ResultCombineSpec, ResultTranslateSpec, ResultTestSpec, ResultAccessSpec, ResultCreationSpec, ResultTransformSpec}

object index extends Specification {
  def is = "Validation specification".title ^ s2"""

  ${"Creating Results" ~/ ResultCreationSpec}
  $p
  ${"Accessing Results" ~/ ResultAccessSpec}
  $p
  ${"Transform Results" ~/ ResultTransformSpec}
  $p
  ${"Testing Results" ~/ ResultTestSpec}
  $p
  ${"Translating Results" ~/ ResultTranslateSpec}
  $p
  ${"Combining Results" ~/ ResultCombineSpec}
  $p
  ${"Miscellaneous Results" ~/ ResultMiscSpec}
  $p
  ${"Unsafe Operations on Results" ~/ ResultUnsafeSpec}

  """

}
