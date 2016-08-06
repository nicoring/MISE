package dmf

import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import dmf.Implicits._

trait SortedBuffer[A] {
  /**
   * Insert an object to the buffer.
   * Returns true if the object was stored, or false if it was rejected immediately
   */  
  def add(x: A): Boolean
  def clear()
  def min: A
  def max: A
  def length: Int
  
  // getter functions
  def iterator: Iterator[A]
  def sortedList: List[A]
}



/**
 * Implementation of a sorted buffer based on Scala's priority queue
 */
class SortedBufferScalaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBuffer[A] {

  import scala.collection.mutable.PriorityQueue
  
  val queue = new PriorityQueue[A]()(ord)

  def add(x: A): Boolean = {
    if (queue.size == size && ord.compare(x, queue.head) > 0) {
      return false 
    } else {
      queue.enqueue(x)
      if (queue.length > size) {
        val y = queue.dequeue()
        if (x == y)
          return false // the inserted element was rejected immediately
      }
      return true
    }
  }
  
  def clear() { 
    queue.clear() 
  }
  
  def min = queue.min
  def max = queue.head
  def length = queue.length
  
  /**
   * Please note: iteration order is undefined (in contrast to ScalaDoc!?)
   */
  def iterator: Iterator[A] = queue.iterator
  def sortedList: List[A] = queue.toList.sorted(ord)
  //def sortedArray: Array[A] = Array(queue.iterator.toSeq :_*).sorted(n)     // doesn't work due to class tag
}

/**
 * Implementation of a sorted buffer based on Java's priority queue
 */
class SortedBufferJavaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBuffer[A] {

  import java.util.PriorityQueue
  import scala.collection.JavaConversions._
  
  val queue = new PriorityQueue[A](10, ord.reverse)     // it looks like the semantics of a Java priority queue is just the other way around, thus reverse the ordering
  
  private[this] var peek: A = queue.peek() 
  
  def add(x: A): Boolean = {
    if (queue.size == size && ord.compare(x, peek) > 0) {
      return false 
    } else {
      queue.add(x)
      if (queue.size > size) {
        val y = queue.poll()    // poll is faster than remove since it is implemented directly in PriorityQueue and not in AbstractQueue, which internally uses poll anyways
        peek = queue.peek()
        if (x == y)
          return false // the inserted element was rejected immediately
        else
          return true
      }
      peek = queue.peek()
      return true
    }
  }
  
  def clear() { 
    queue.clear() 
  }
  
  def min = queue.iterator.min
  def max = queue.peek
  def length = queue.size
  
  /**
   * Please note: iteration order is undefined (in contrast to ScalaDoc!?)
   */
  def iterator: Iterator[A] = queue.iterator
  def sortedList: List[A] = queue.toList.sorted(ord)
}

/**
 * General optimized version, which uses either 
 * - the Java implementation or 
 * - implementations for fixed sizes
 */
class SortedBufferOptimized[A](size: Int)(implicit ord: Ordering[A]) extends SortedBuffer[A] {
  val underlying = size match {
    case 1 => new SortedBufferOptimizedK1()(ord)
    case _ => new SortedBufferJavaImpl[A](size)(ord)
  }
  def add(x: A): Boolean = {
    underlying.add(x)
  }
  def clear() { 
    underlying.clear() 
  }
  def min = underlying.min
  def max = underlying.max
  def length = underlying.length
  def iterator: Iterator[A] = underlying.iterator
  def sortedList: List[A] = underlying.sortedList
}
/**
 * Optimized version for k=1
 */
class SortedBufferOptimizedK1[A]()(implicit ord: Ordering[A]) extends SortedBuffer[A] {
  private[this] var best1: Option[A] = None 
  def add(x: A): Boolean = {
    if (best1.isEmpty || ord.compare(x, best1.get) < 0) {
      best1 = Some(x)
      return true 
    } else {
      return false
    }
  }
  def clear() { 
    best1 = None
  }
  def min = best1.get
  def max = best1.get
  def length = if (best1.isDefined) 1 else 0
  def iterator: Iterator[A] = best1.toIterator
  def sortedList: List[A] = best1.toList
}



/**
 * For convenience: meaningful name to make sure whether high or low values are kept - LOW version
 * Currently active implementation: 
 */
class SortedBufferKeepLow[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferJavaImpl[A](size)(ord) 
/** Explicit names of alternative implementations */
class SortedBufferKeepLowScalaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferScalaImpl[A](size)(ord) 
class SortedBufferKeepLowJavaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferJavaImpl[A](size)(ord) 
class SortedBufferKeepLowOptimized[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferOptimized[A](size)(ord) 

/**
 * For convenience: meaningful name to make sure whether high or low values are kept - HIGH version 
 * Note: There seems to be a nasty bug that the reversed ordering is not implemented consistently in PriorityQueue
 * (reversed ordering actually also reverses the definition of "min" and "max" so a queue with values
 * 1.0 2.0 3.0 would have a "max" of 1.0 and a "min" of 3.0 due to the reversed ordering (scala 2.9.1)
 * http://www.scala-lang.org/node/9021
 */
class SortedBufferKeepHigh[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferJavaImpl[A](size)(ord.reverse) {
  override def min = super.max
  override def max = super.min
}
/** Explicit names of alternative implementations */
class SortedBufferKeepHighScalaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferScalaImpl[A](size)(ord.reverse) {
  override def min = super.max
  override def max = super.min
}
class SortedBufferKeepHighJavaImpl[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferJavaImpl[A](size)(ord.reverse) {
  override def min = super.max
  override def max = super.min
}
class SortedBufferKeepHighOptimized[A](size: Int)(implicit ord: Ordering[A]) extends SortedBufferOptimized[A](size)(ord.reverse) {
  override def min = super.max
  override def max = super.min
}

/**
 * For convenience: implementation for NN orderings 
 */
class NNBuffer(size: Int)(implicit manifest: ClassTag[Neighbor]) extends SortedBufferKeepLow[Neighbor](size)(new Ordering[Neighbor] {
  def compare(a: Neighbor, b: Neighbor) = math.Ordering.Double.compare(a.dist, b.dist)
})


/**
 * Since the sorted buffer tests also contain a lot of benchmarking, this is not stored as a ScalaTest 
 */
object TestSortedBuffer {
  def main(args: Array[String]) {
    val iterations = 1000
    val streamLengths = List(100, 1000, 10000)
    val bufferSizes = List(1, 5, 10, 100)
    
    val bufferImplementations = 
      ("Scala",     "L", (size: Int) => new SortedBufferKeepLowScalaImpl[Double](size)) :: 
      ("Java",      "L", (size: Int) => new SortedBufferKeepLowJavaImpl[Double](size)) :: 
      ("Optimized", "L", (size: Int) => new SortedBufferKeepLowOptimized[Double](size)) :: 
      ("Scala",     "H", (size: Int) => new SortedBufferKeepHighScalaImpl[Double](size)) :: 
      ("Java",      "H", (size: Int) => new SortedBufferKeepHighJavaImpl[Double](size)) :: 
      ("Optimized", "H", (size: Int) => new SortedBufferKeepHighOptimized[Double](size)) :: 
      Nil
    
    for (streamLength <- streamLengths) {
      for (bufferSize <- bufferSizes) {
        println(f"k = $bufferSize")
        val insertTimes = Array.tabulate(bufferImplementations.length)(_ => ArrayBuffer[Double]())
        val queryTimes = Array.tabulate(bufferImplementations.length)(_ => ArrayBuffer[Double]())
        for (iter <- Range(0, iterations)) {
          val data = Array.tabulate(streamLength)(_ => Random.nextDouble)
          val refDataL = data.sorted.take(bufferSize).toSet
          val refDataH = data.sorted.reverse.take(bufferSize).toSet
          val buffers = bufferImplementations.map(a => a._3(bufferSize))
          val boolFillInfos = bufferImplementations.indices.map { i =>
            val buffer = buffers(i)
            val t1 = System.nanoTime()
            val boolFillInfo = data.map(x => buffer.add(x))
            val t2 = System.nanoTime()
            val insertTime = (t2-t1).toDouble /* unit: ns */
            insertTimes(i) += insertTime
            boolFillInfo
          }
          // TODO: assert bool fill info is the same
          // check that min/max give expected results
          for (i <- bufferImplementations.indices) {
            val buffer = buffers(i)
            if (bufferImplementations(i)._2 == "L") {
              assert(buffer.min == refDataL.min)
              assert(buffer.max == refDataL.max)
            } else {
              assert(buffer.min == refDataH.min)
              assert(buffer.max == refDataH.max)
            }
          }
          for (i <- bufferImplementations.indices) {
            val buffer = buffers(i)
            val t1 = System.nanoTime()
            val topK = buffer.iterator.toArray
            val t2 = System.nanoTime()
            val queryTime = (t2-t1).toDouble /* unit: ns */
            queryTimes(i) += queryTime
            assert(topK.length == bufferSize)
            if (bufferImplementations(i)._2 == "L") {
              //println(i, topK.toSet, refDataL)
              assert(topK.toSet == refDataL)
            } else {
              //println(i, topK.toSet, refDataH)
              assert(topK.toSet == refDataH)
            }
          }
        }
        // print results:
        for (i <- bufferImplementations.indices) {
          val name = bufferImplementations(i)._1 + "_" + bufferImplementations(i)._2
          val avgInsertTime = insertTimes(i).map(_ / streamLength).median
          val avgQueryTime = queryTimes(i).median
          println(f"stream length: $streamLength%8d    k: $bufferSize%8d    implementation: $name%-30s    median insert time (per single insert): $avgInsertTime%16.3f ns    median query time: $avgQueryTime%16.3f ns")
        }
      }
    }
  }
}



/**
------------------------------------------------------------------------------------
Older version of the code above; 
only stored to keep sample code uses ClassManifests
------------------------------------------------------------------------------------

class SortedBufferScalaImpl[A](size: Int)(implicit n: Ordering[A]/*, manifest: ClassManifest[A]*/) {

  import scala.collection.mutable.PriorityQueue
  
  val queue = new PriorityQueue[A]()(n)

  /**
   * Insert an object to the buffer.
   * Returns true if the object was stored, or false if it was rejected immediately
   */
  def add(x: A): Boolean = {
    if (queue.size == size && n.compare(x, queue.head) > 0) {
      return false 
    } else {
      queue.enqueue(x)
      if (queue.length > size) {
        val y = queue.dequeue()
        if (x == y)
          return false // the inserted element was rejected immediately
      }
      return true
    }
  }
  
  def clear() { 
    queue.clear() 
  }
  
  def min = queue.min
  def max = queue.head
  def length = queue.length
  
  /**
   * Please note: iteration order is undefined (in contrast to ScalaDoc!?)
   */
  def iterator: Iterator[A] = queue.iterator
  
  def sortedList: List[A] = queue.toList.sorted(n)
  
  //def sortedArray: Array[A] = queue.iterator.toArray.sorted(n)
}

class SortedBufferKeepHigh[A](size: Int)(implicit n: Ordering[A]/*, manifest: ClassManifest[A]*/) extends SortedBufferScalaImpl[A](size)(new Ordering[A] {
  def compare(a: A, b: A) = n.compare(b, a)
}/*, manifest*/) {
  /**
   * There seems to be a nasty bug that the reversed ordering is not implemented consistently in PriorityQueue
   * (reversed ordering actually also reverses the definition of "min" and "max" so a queue with values
   * 1.0 2.0 3.0 would have a "max" of 1.0 and a "min" of 3.0 due to the reversed ordering (scala 2.9.1)
   * http://www.scala-lang.org/node/9021
   */
  override def min = super.max
  override def max = super.min
}

class SortedBufferKeepLow[A](size: Int)(implicit n: Ordering[A]/*, manifest: ClassManifest[A]*/) extends SortedBufferScalaImpl[A](size)(new Ordering[A] {
  def compare(a: A, b: A) = n.compare(a, b)
}/*, manifest*/) 
class NNBuffer(size: Int)(implicit manifest: ClassTag[Neighbor]) extends SortedBufferKeepLow[Neighbor](size)(new Ordering[Neighbor] {
  def compare(a: Neighbor, b: Neighbor) = math.Ordering.Double.compare(a.dist, b.dist)
}/*, manifest*/)


------------------------------------------------------------------------------------
Some really really old code:
------------------------------------------------------------------------------------

trait SortedBufferMap[A, B] {
  def add(key: A, value: B)
  def clear()
  def iterator: Iterator[(A, B)] 
}

object SortedBufferMap {
  def apply[A: Ordering, B](size: Int) = new SortedBufferMapV1(size)
}

class SortedBufferMapV1[A: Ordering, B](size: Int) extends SortedBufferMap[A, B] {

  import scala.collection.mutable._
  val m = new HashMap[A, B] with MultiMap[A, B]

  def add(key: A, value: B): Unit = { 
    m += key -> value
    if (m.size > size)
      m -= m.keys.max
  }
  
  def clear() { m.clear() }
  
  def iterator = new Iterator[(A, B)] {
    
  }
}

class CrappyBuffer[A: Ordering, B](size: Int) {

  import scala.collection.mutable._
  val m = new HashMap[A, Set[B]] with MultiMap[A, B]

  def add(key: A, value: B): Unit = { 
    //m += key -> value
    m.addBinding(key, value)
    if (m.size > size)
      m -= m.keys.max
  }
  
  def clear() { m.clear() }
  
  def iterator: Iterator[(A, B)] = m.iterator
}



class CrappyBuffer[A, B](size: Int)(implicit n : Ordering[A]) {

  var m = List[(A,B)]()

  def add(key: A, value: B): Unit = { 
    //m += key -> value
    m = ((key,value) :: m).sortWith((a1, a2) => n.gt(a1._1, a2._1)).take(size)
  }
  
  def clear() { m = List.empty }
  
  def iterator: Iterator[(A, B)] = m.iterator
}
*/


object DummyObjectToPreventEclipseBug





