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
class ResidualTree[T] {

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

  private def findNode(x: Int): Option[Node[T]] = {
    var node: Node[T] = root
    var maxCanBeEncoded = 1
    var value = node.value
    val iterator = baseGenerator.iterator
    while (x >= maxCanBeEncoded) {
      var currentBase = node.base
      node = node.getChildNode(x % iterator.next()) match {
        case Some(x) => x
        case None => return None
      }
      maxCanBeEncoded *= node.base
    }
    Some(node)
  }

  def add(x: Int,value:T) {
    var node: Node[T] = root
    var maxCanBeEncoded = 1
    val iterator = baseGenerator.iterator
    var mod = 0
    while (x >= maxCanBeEncoded) { //TODO: what if x <= 0?
      val parent = node
      var currentBase = node.base
      val nextSimple = iterator.next()
      mod = x % nextSimple
      
      node = parent.getChildNode(mod) match {
        case Some(x) => x
        case None => {
          val newNode = new Node[T](nextSimple, None)
          parent.setChildNode(mod, newNode)
          newNode
        }
      }
      maxCanBeEncoded *= node.base
    }
    node.value = Some(value)

//    println("base " + node.base)
  }

  def remove(x: Int) {
    findNode(x) match {
      case Some(node) => node.value = None
      case None =>
    }
  }

  class Root extends Node[T](1, None)

  class Node[T](val base: Int, var value: Option[T]) {
    private val child: Array[Node[T]] = new Array(baseGenerator.nextSimple(base))

    def setChildNode(mod: Int, node: Node[T]) {
      child(mod) = node
    }

    def getChildNode(mod: Int): Option[Node[T]] = {
      child(mod) match {
        case null => None
        case node: Node[T] => Some(node)
      }
    }

    def removeChildNode(mod: Int) {
      child(mod) = null
    }

    override def toString: String = "Node[base=" + base + ", value=" + value + "]"

  }

}
