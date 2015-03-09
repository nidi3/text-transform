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

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.Name._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait PseudoSegment

class AttributePair[T](name: Attribute[T], value: T) extends Pair(name, value) with PseudoSegment

/**
 * A Segment is an abstract representation of a text. It is produced by a `guru.nidi.text.transform.Parser`
 * and consumed by a `guru.nidi.text.transform.Formatter`
 * A Segment is defined by its name, a map of attributes and a list of child segments.
 */
class Segment(val name: Name) extends PseudoSegment {
  val attributes = mutable.Map[Attribute[_], Any]()
  val children = ListBuffer[Segment]()
  private var _parent: Option[Segment] = None

  def apply[T](name: Attribute[T]): Option[T] = attributes.get(name) match {
    case value: Some[T] => value
    case _ => None
  }

  def apply(values: PseudoSegment*) = add(values: _*)

  def parent = this._parent

  def addTo(parent: Segment) = {
    parent.add(this)
    this
  }

  def add(values: PseudoSegment*): Segment = {
    values.foreach(_ match {
      case seg: Segment =>
        seg._parent = Some(this)
        children += seg
      case attr: AttributePair[_] =>
        attributes.put(attr._1, attr._2)
    })
    this
  }

  def prepend(child: Segment): Segment = {
    child._parent = Some(this)
    child +=: children
    this
  }

  def removeAll(p: Segment => Boolean): Segment = {
    var i = 0
    while (i < children.length) {
      if (p(children(i))) {
        children.insert(i, children.remove(i).removeAll(p).children: _*)
      } else {
        i += 1
      }
    }
    this
  }

  def addAttribute[T](name: Attribute[T], value: T): Segment = add(new AttributePair[T](name, value))

  def addChild(child: Segment): Segment = add(child)

  def inherited[T](name: Attribute[T]): Option[T] = {
    this(name) match {
      case Some(value) => Some(value)
      case _ => if (parent.isEmpty) None else parent.get.inherited(name)
    }
  }

  def findParent(name: Name): Option[Segment] = {
    if (this.name == name) Some(this)
    else if (parent.isEmpty) None
    else parent.get.findParent(name)
  }

  def root: Segment = {
    parent match {
      case Some(seg) => seg.root
      case None => this
    }
  }

  override def toString = toFormattedString(0)

  private def toFormattedString(level: Int): String = {
    def indent = "  " * level
    def ch = {
      if (children.isEmpty) {
        ""
      } else {
        ", children=\n" +
          children.map(child => child.toFormattedString(level + 1)).mkString("\n") +
          indent
      }
    }
    indent + s"{$name, $attributes $ch}"
  }

  override def equals(other: Any) = {
    if (!other.isInstanceOf[Segment]) false
    else {
      val s = other.asInstanceOf[Segment]
      s.attributes == attributes && s.children == children
    }
  }

  override def hashCode = {
    attributes.hashCode + children.hashCode * 31
  }

}

object Segment {
  def apply(name: Name, children: PseudoSegment*): Segment = new Segment(name)(children: _*)

  def plain(text: String): Segment = PLAIN(TEXT -> text)

  def symbol(original: String, typ: AttributeValue): Segment = SYMBOL(ORIGINAL -> original, TYPE -> typ)
}
