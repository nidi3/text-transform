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


import java.text.MessageFormat
import java.util.{Locale, ResourceBundle}

import guru.nidi.text.transform.Name.ROOT
import guru.nidi.text.transform.Segment.plain

/**
 *
 */
class TransformContext(val headingLevel: Int, locale: Locale, resourceLoader: ResourceLoader) {
  val messages = ResourceBundle.getBundle(classOf[TransformContext].getPackage.getName + ".messages", locale)

  def message(key: String, formatter: Formatter, parameters: AnyRef*) =
    MessageFormat.format(messages.getString(key), parameters.map(v => formatter.format(ROOT(plain(v.toString)))).toArray: _*)

  def loadResource(source: Segment, name: String): Option[String] = resourceLoader.loadResource(source, name)

  def subContext = new TransformContext(headingLevel + 1, locale, resourceLoader)

  def includeSub(parent: Segment, sub: Segment) = sub

  def processLink(source: Segment, text: String, url: String): Pair[String, String] = (text, url)
}


