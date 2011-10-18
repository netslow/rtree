/**
 *
 */
package org.halfcup.tree

import scala.collection.mutable.ListBuffer


/**
 * @author Stas Pozdnyakov
 *
 */
class ResidualTreePar[T] {

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

  def findBase(x:Int):List[Int] = {
	var maxCanBeEncoded = 1
	var base = 1
	var buffer = new ListBuffer[Int]
	while(x >= maxCanBeEncoded){
	  base = baseGenerator.nextSimple(base)
	  buffer + base
	  maxCanBeEncoded *= base
	}
	buffer.toList
  }
  
  private def findNode(x: Int): Option[Node[T]] = {
    var node: Node[T] = root
    val route = findBase(x).par.map(x % _)
    
    for( modulus <- route.seq){
      val parent = node
       node = parent.getChildNode(modulus) match {
        case Some(x) => x
        case None => return None
      }
    }
    
    Some(node)
  }

  def add(x: Int,value:T) {
    var node: Node[T] = root
    
    val route = findBase(x).par.map(x % _)
    var base = 1
    for( modulus <- route.seq){
      val parent = node
       node = parent.getChildNode(modulus) match {
        case Some(x) => x
        case None => {
          val newNode = new Node[T](baseGenerator.nextSimple(base), None)
          parent.setChildNode(modulus, newNode)
          newNode
        }
      }
      base = node.base
    }
    node.value = Some(value)
    
    println("base " + node.base)
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