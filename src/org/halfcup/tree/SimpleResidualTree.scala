/**
 *
 */
package org.halfcup.tree

/**
 * 
 * @author Stas Pozdnyakov
 * @version 0.1
 *
 */
class SimpleResidualTree {

  private val baseGenerator: SimpleNumbersGenerator = Generator

  private val root = new Root

  def contains(x: Int): Boolean = {
    findNode(x) match {
      case Some(node) => {
        node.value match {
          case Some(value) => true
          case None => false
        }
      }
      case None => false
    }
  }

  private def findNode(x: Int): Option[Node] = {
    var node: Node = root
    var maxCanBeEncoded = 1
    var value = node.value
    while (x >= maxCanBeEncoded) {
      var currentBase = node.base
      node = node.getChildNode(x % baseGenerator.nextSimple(currentBase)) match {
        case Some(x) => x
        case None => return None
      }
      maxCanBeEncoded *= node.base
    }
    Some(node)
  }

  def add(x: Int) {
    var node: Node = root
    var maxCanBeEncoded = 1
    while (x >= maxCanBeEncoded) { //TODO: what if x <= 0?
      val parent = node

      var currentBase = node.base
      node = parent.getChildNode(x % baseGenerator.nextSimple(currentBase)) match {
        case Some(x) => x
        case None => {
          val newNode = new Node(baseGenerator.nextSimple(currentBase), None)
          parent.setChildNode(x % newNode.base, newNode)
          newNode
        }
      }
      maxCanBeEncoded *= node.base
    }
    node.value = Some(x)

    println("base " + node.base)
  }

  def remove(x: Int) {
    findNode(x) match {
      case Some(node) => node.value = None
      case None =>
    }
  }

  class Root extends Node(1, None)

  class Node(val base: Int, var value: Option[Int]) {
    private val child: Array[Node] = new Array(baseGenerator.nextSimple(base))

    def setChildNode(mod: Int, node: Node) {
      child(mod) = node
    }

    def getChildNode(mod: Int): Option[Node] = {
      child(mod) match {
        case null => None
        case node: Node => Some(node)
      }
    }

    def removeChildNode(mod: Int) {
      child(mod) = null
    }

    override def toString: String = "Node[base=" + base + ", value=" + value + "]"

  }

}
/*
trait SimpleNumbersGenerator {
  def nextSimple(x: Int): Int
}

object Generator extends SimpleNumbersGenerator {
  // just for test
  private val simpleNumbersList: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
  

  def nextSimple(x: Int): Int = {
    simpleNumbersList.find(_ > x) match {
      case Some(x) => x
      case None => throw new Exception("Can't get next simple number")
    }
  }
}

*/
