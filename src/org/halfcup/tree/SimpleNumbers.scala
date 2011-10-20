/**
 *
 */
package org.halfcup.tree

/**
 * @author Stas Pozdnyakov
 *
 */

trait SimpleNumbersGenerator {
  def nextSimple(x: Int): Int
  def iterator:Iterator[Int]
//  def nextSimple(x: Long): Long
}

object Generator extends SimpleNumbersGenerator {
  // just for test
  private val simpleNumbersList: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
  
//  private val numbersLong: List[Long] = List[Long](2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
  def iterator = simpleNumbersList.iterator  

  def nextSimple(x: Int): Int = {
    simpleNumbersList.find(_ > x) match {
      case Some(x) => x
      case None => throw new Exception("Can't get next simple number")
    }
  }
  
  
//  def nextSimple(x: Long): Long = {
//    numbersLong.find(_ > x) match {
//      case Some(x) => x
//      case None => throw new Exception("Can't get next simple number")
//    }
//  }
}


