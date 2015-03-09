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
package guru.nidi.text.transform.parse

import guru.nidi.text.transform.{Parser, Segment, TransformContext}

/**
 *
 */
abstract class AbstractParser(val context: TransformContext) extends Parser {

  private var _input: String = ""

  def parse(input: String): Segment = {
    _input = input
    parseImpl()
  }

  def parseImpl(): Segment

  def parseSub(sub: String, subContext: TransformContext = context): Segment = newInstance(subContext).parse(sub)

  protected def newInstance(subContext: TransformContext = context): AbstractParser = {
    try {
      getClass.getConstructor(classOf[TransformContext]).newInstance(subContext)
    } catch {
      case e: Exception => throw new RuntimeException("Could not create a new instance of " + getClass, e)
    }
  }

  def input = _input
}
