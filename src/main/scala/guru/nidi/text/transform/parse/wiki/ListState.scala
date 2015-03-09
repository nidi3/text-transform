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
package guru.nidi.text.transform.parse.wiki

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.{AttributeValue, Segment}


/**
 * Holds informations about the list element we are currently parsing.
 */
class ListState {
  var currentList: Segment = null

  def isInList = currentList != null

  def getLevel: Int = currentList(LEVEL).get

  def matchesListType(listType: AttributeValue) = currentList(TYPE).get == listType

  def reset() {
    currentList = null
  }

  def gotoParent() {
    currentList =
      currentList.parent match {
        case Some(p) if (p.name == LIST) => p
        case _ => null
      }
  }

  def gotoChild(list: Segment) {
    currentList = list.addTo(currentList)
  }

}
