package stni.text.transform.parse.wiki

import stni.text.transform.{Segment, AttributeValue}
import stni.text.transform.Attribute._
import stni.text.transform.Name._
import stni.text.transform.parse.AbstractCharReadingParser.EOI


/**
 * Parse ordered and unordered lists.
 */
class ListParser(parser: AbstractWikiParser,listState:ListState, listType: AttributeValue) {
  def list(sym: Char) {
    val level = parser.getCount(sym)
    gotoLevel(level)
    if (!listState.matchesListType(listType)) {
      createSiblingOfCurrent()
    }
    createListItem()
    handleNewlines()
  }

  private def gotoLevel(level: Int) {
    if (listState.isInList) {
      findListOfLevel(level)
    } else {
      createNewCurrentList(level)
    }
  }

  private def findListOfLevel(level: Int) {
    val currentLevel = listState.getLevel
    if (currentLevel < level) {
      addChildLists(level)
    } else if (currentLevel > level) {
      for (l <- currentLevel until level by -1) {
        listState.gotoParent()
      }
    }
  }

  private def addChildLists(level: Int) {
    val currentLevel = listState.getLevel
    for (l <- currentLevel + 1 to level) {
      listState.gotoChild(LIST(TYPE -> listType, LEVEL -> l))
    }
  }

  private def createNewCurrentList(level: Int) {
    addFirstLevelList()
    addChildLists(level)
  }

  private def addFirstLevelList() {
    listState.currentList = LIST(TYPE -> listType, LEVEL -> 1)
    parser.addToResult(listState.currentList)
  }

  private def createSiblingOfCurrent() {
    listState.gotoParent()
    if (listState.isInList) {
      addChildLists(listState.getLevel + 1)
    } else {
      addFirstLevelList()
    }
  }

  private def createListItem() {
    val content = new StringBuilder(parser.readUntil("\n"))
    while (!parser.isCurrentCharOneOf("#*\n" + EOI)) {
      content.append("\n").append(parser.readUntil("\n"))
    }
    listState.currentList(ITEM(parser.parseSub(content.toString()).children: _*))
  }

  private def handleNewlines() {
    while (parser.currentChar == '\n') {
      listState.reset()
      parser.nextChar()
    }
  }
}
