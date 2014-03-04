package guru.nidi.text.transform.parse.wiki

import guru.nidi.text.transform.{AttributeValue, Segment}
import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.Name._


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
