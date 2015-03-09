/**
 * Copyright (C) 2013 Stefan Niederhauser (nidin@gmx.ch)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package guru.nidi.text.transform

import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Segment._
import org.scalatest.FlatSpec


/**
 *
 */
class SegmentTest extends FlatSpec {
  behavior of "removeAll"

  it should "remove all elements matching the condition" in {
    assert(ROOT(plain("a"), BOLD(plain("b")), plain("c")).removeAll(_.name == BOLD) ===
      ROOT(plain("a"), plain("b"), plain("c")))
  }

  it should "remove all two following elements matching the condition" in {
    assert(ROOT(plain("a"), BOLD(plain("b")), BOLD(plain("b")), plain("c")).removeAll(_.name == BOLD) ===
      ROOT(plain("a"), plain("b"), plain("b"), plain("c")))
  }

  it should "remove all two following empty elements matching the condition" in {
    assert(ROOT(plain("a"), NEWLINE(), NEWLINE(), plain("c")).removeAll(_.name == NEWLINE) ===
      ROOT(plain("a"), plain("c")))
  }
}
