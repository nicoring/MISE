package dmf.stream.mutinf


import scala.util.Random
import dmf.Helpers
import dmf.SortedBufferKeepLow
import dmf.Stopwatch
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem
import dmf.Implicits._
import java.util.PriorityQueue
import dmf.SortedBufferKeepLowScalaImpl
import dmf.BinarySearch

trait NeighborSummary2D {
  def addNeighbor(neighborX: Double, neighborY: Double, forward: Boolean)
  def getNeighborhood(nL: Int, nR: Int): (Int, Int, Double, Int, Int)
}

trait FactoryNeighborSummary2D {
  def build(x: Double, y: Double, k: Int): NeighborSummary2D
  val description: String = this.getClass.getSimpleName
  override def toString() = description 
}


/**
 * Trivial implementation: build neighborhood for each query
 */
class NeighborSummary2DReference(val x: Double, val y: Double, k: Int) extends NeighborSummary2D {
  
  val dataL = new collection.mutable.ArrayBuffer[(Double, Double)]()
  val dataR = new collection.mutable.ArrayBuffer[(Double, Double)]()
  
  def addNeighbor(neighborX: Double, neighborY: Double, forward: Boolean) {
    if (forward) {
      dataR += ((neighborX, neighborY))
    } else {
      dataL += ((neighborX, neighborY))
    }
  }
  
  def getNeighborhood(nL: Int, nR: Int): (Int, Int, Double, Int, Int) = {
    if (nL+nR < k)
      return (0, 0, 0.0, 0, 0)
    else {
      val sampleL = dataL.take(nL)
      val sampleR = dataR.take(nR)
      val sample = sampleL ++ sampleR
      val distances = sample.map(p => math.abs(x-p._1) max math.abs(y-p._2)).sorted
      val kNeighborDist = distances(k-1)
      val nx = sample.filter(p => math.abs(x-p._1) < kNeighborDist).length
      val ny = sample.filter(p => math.abs(y-p._2) < kNeighborDist).length
      //println(f"kNeighborDist = $kNeighborDist%10.6f")
      return (nx, ny, kNeighborDist, sampleL.length, sampleR.length)
    }
  }
  
}
class FactoryNeighborSummary2DReference extends FactoryNeighborSummary2D {
  def build(x: Double, y: Double, k: Int) = new NeighborSummary2DReference(x, y, k)
}



/**
 * Incremental implementation V1
 * Only works with k=1
 */
class NeighborSummary2DIncremental_v1(val x: Double, val y: Double) extends NeighborSummary2D {
  
  val nearestNeighborsL = new SortedBufferKeepLow[Double](1)
  val nearestNeighborsR = new SortedBufferKeepLow[Double](1)
  val xNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val xNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()

  val kNeighborDistsR = new collection.mutable.ArrayBuffer[(Int, Double)]()
  val kNeighborDistsL = new collection.mutable.ArrayBuffer[(Int, Double)]()
  kNeighborDistsR += ((0, Double.MaxValue))
  kNeighborDistsL += ((0, Double.MaxValue))
  var tL = 0
  var tR = 0
  
  def addNeighbor(neighborX: Double, neighborY: Double, forward: Boolean) {
    val xDist = math.abs(x-neighborX)
    val yDist = math.abs(y-neighborY)
    val dist =  xDist max yDist
    if (forward) {
      if (nearestNeighborsR.add(dist) && nearestNeighborsR.length == 1) {
        kNeighborDistsR += ((tR, nearestNeighborsR.max))
      }
      if (xDist < nearestNeighborsR.max) {
        xNeighborsR.enqueue((xDist, tR))
      }
      if (yDist < nearestNeighborsR.max) {
        yNeighborsR.enqueue((yDist, tR))
      }
      //println(f"after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDists.last._2}%10.6f    ${nearestNeighbors.sortedList}    ${xNeighbors.toList.sorted}    ${yNeighbors.toList.sorted}")
      tR += 1
    } else {
      if (nearestNeighborsL.add(dist) && nearestNeighborsL.length == 1) {
        kNeighborDistsL += ((tL, nearestNeighborsL.max))
      }
      if (xDist < nearestNeighborsL.max) {
        xNeighborsL.enqueue((xDist, tL))
      }
      if (yDist < nearestNeighborsL.max) {
        yNeighborsL.enqueue((yDist, tL))
      }
      //println(f"after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDists.last._2}%10.6f    ${nearestNeighbors.sortedList}    ${xNeighbors.toList.sorted}    ${yNeighbors.toList.sorted}")
      tL += 1
    }
  }
  
  def getNeighborhood(nL: Int, nR: Int): (Int, Int, Double, Int, Int) = {
    if (nL+nR < 1)
      return (0, 0, 0.0, 0, 0)
    else {
      // the following determination of the kNeighborDist works but is unnecessarily complex:
      // the first step collects _all_ neighbor-distance-timestamps in the window (taking the _last_ would actually suffice, see other implementations)
      // but since the following step again takes the minimum of all these, the result is obviously correct 
      val kNeighborDistL = kNeighborDistsL.filter(_._1 < nL)
      val kNeighborDistR = kNeighborDistsR.filter(_._1 < nR)
      val kNeighborDist = (kNeighborDistL ++ kNeighborDistR).minBy(_._2)._2
      val nxL = xNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nyL = yNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nxR = xNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      val nyR = yNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      /*
      println(nL, nR, 
          xNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL), 
          yNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL),
          xNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR),
          yNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR))
      */
      //println(f"kNeighborDist = $kNeighborDist%10.6f    ${xNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}    ${yNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}")
      return (nxL+nxR, nyL+nyR, kNeighborDist, tL min nL, tR min nR)
    }
  }
  
  def printInfo() {
    //println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
  
}
class FactoryNeighborSummary2DIncremental_v1 extends FactoryNeighborSummary2D {
  def build(x: Double, y: Double, k: Int) = new NeighborSummary2DIncremental_v1(x, y)
}



/**
 * Incremental implementation V2
 * works with arbitrary k values
 */
class NeighborSummary2DIncremental_v2(val x: Double, val y: Double, k: Int) extends NeighborSummary2D {
  
  val nearestNeighborsL = new SortedBufferKeepLowScalaImpl[Double](k)
  val nearestNeighborsR = new SortedBufferKeepLowScalaImpl[Double](k)
  val xNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val xNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()

  val kNeighborDistsR = new collection.mutable.ArrayBuffer[(Int, Set[Double])]()
  val kNeighborDistsL = new collection.mutable.ArrayBuffer[(Int, Set[Double])]()
  kNeighborDistsR += ((-1, Set()))
  kNeighborDistsL += ((-1, Set()))
  var tL = 0
  var tR = 0
  
  def addNeighbor(neighborX: Double, neighborY: Double, forward: Boolean) {
    val xDist = math.abs(x-neighborX)
    val yDist = math.abs(y-neighborY)
    val dist =  xDist max yDist
    if (forward) {
      if (nearestNeighborsR.add(dist)) {
        kNeighborDistsR += ((tR, nearestNeighborsR.sortedList.toSet))
      }
      /**
       * Note that the following comparison must be "<=" and not "<".
       * This is not the actual comparison, which determines the marginal counts (in this case it really must be "<", see getNeighborhood, where we indeed compare against "< kNeighborDist")
       * Here, the question is only whether we store the point at all and there is a problem when there are not yet k nearest neighbors collected:
       * In this case the object that will be returned by currentNNR.max is not the kNN, but simply the current object with largest distance! 
       * Example: let k = 10 and we add the 6th element to the right, so currentNNR.max corresponds only to the 6th NN.
       * We have to store the marginal-coordinate in any case since when as soon as we reach a total of 10 neighbors, the k-dist suddenly is defined, and now all of the 9 points we have seen before might or might not be within the marginal slices.
       * An alternative would be to check for something like (tR < k || xDist < current.max).
       * This would save some memory for cases >k (where we no longer would have to store the point) but might be slower due to the added condition...  
       */
      if (xDist <= nearestNeighborsR.max) {
        xNeighborsR.enqueue((xDist, tR))
      }
      if (yDist <= nearestNeighborsR.max) {
        yNeighborsR.enqueue((yDist, tR))
      }
      //println(f"after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDists.last._2}%10.6f    ${nearestNeighbors.sortedList}    ${xNeighbors.toList.sorted}    ${yNeighbors.toList.sorted}")
      tR += 1
    } else {
      if (nearestNeighborsL.add(dist)) {
        kNeighborDistsL += ((tL, nearestNeighborsL.sortedList.toSet))
      }
      if (xDist <= nearestNeighborsL.max) {
        xNeighborsL.enqueue((xDist, tL))
      }
      if (yDist <= nearestNeighborsL.max) {
        yNeighborsL.enqueue((yDist, tL))
      }
      //println(f"after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDists.last._2}%10.6f    ${nearestNeighbors.sortedList}    ${xNeighbors.toList.sorted}    ${yNeighbors.toList.sorted}")
      tL += 1
    }
  }
  
  def getNeighborhood(nL: Int, nR: Int): (Int, Int, Double, Int, Int) = {
    if (nL+nR < k)
      return (0, 0, 0.0, 0, 0)
    else {
      val kNeighborDistL = kNeighborDistsL.filter(_._1 < nL).last._2
      val kNeighborDistR = kNeighborDistsR.filter(_._1 < nR).last._2
      val kNeighborDist = (kNeighborDistL ++ kNeighborDistR).toList.sorted.apply(k-1)
      val nxL = xNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nyL = yNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nxR = xNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      val nyR = yNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      /*
      println(nL, nR, kNeighborDistsL.toList, kNeighborDistsR.toList)
      println(nL, nR, 
          xNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL), 
          yNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL),
          xNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR),
          yNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR))
      */
      //println(f"kNeighborDist = $kNeighborDist%10.6f    ${xNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}    ${yNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}")
      return (nxL+nxR, nyL+nyR, kNeighborDist, tL min nL, tR min nR)
    }
  }
  
  def printInfo() {
    //println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
}
class FactoryNeighborSummary2DIncremental_v2 extends FactoryNeighborSummary2D {
  def build(x: Double, y: Double, k: Int) = new NeighborSummary2DIncremental_v2(x, y, k)
}



/**
 * Incremental implementation V3
 * works with arbitrary k values
 */
class NeighborSummary2DIncremental_v3(val x: Double, val y: Double, k: Int) extends NeighborSummary2D {
  
  //val debugOut = Helpers.outputFile("ns_debug.log")
  
  // storing of "marginal points"
  val xNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val xNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsL = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighborsR = new collection.mutable.PriorityQueue[(Double, Int)]()

  // the current nearest neighbors 
  val currentNNL = new SortedBufferKeepLow[Double](k) // note: Java implementation is slightly slower for k=1, no: but runtime has much larger variance
  val currentNNR = new SortedBufferKeepLow[Double](k)
  // to store the NN history
  val kNeighborDistsR = new collection.mutable.ArrayBuffer[(Int, Set[Double])]() // initialSize=128    changing initialSize (from default 16) does not seem to have a big effect
  val kNeighborDistsL = new collection.mutable.ArrayBuffer[(Int, Set[Double])]()
  kNeighborDistsR += ((-1, Set()))
  kNeighborDistsL += ((-1, Set()))
  var tL = 0
  var tR = 0
  
  /**
   * The performance of an insert is determined by:
   * - insert of new point to current NN, implemented by Java Priority Queue => good performance, TODO: an optimized k=1 implementation
   * - if insert of new point to current NN is successful, store in "NN history" => currently ArrayBuffer => excellent performance
   * - if marginal within slice (max distance should be obtained in O(1)): add to "marginal points" => currently Scala priority queue => not too much to gain, on TODO for queries anyways...
   */
  def addNeighbor(neighborX: Double, neighborY: Double, forward: Boolean) {
    val xDist = math.abs(x-neighborX)
    val yDist = math.abs(y-neighborY)
    val dist =  xDist max yDist
    if (forward) {
      if (currentNNR.length < k) {
        /**
         * If we do not yet have k neighbors we can get rid of all conditions, since we have to store everything anyway
         * See previous implementation for an explanation (where this was solved by a lower-or-equal condition)
         */
        currentNNR.add(dist)
        kNeighborDistsR += ((tR, currentNNR.iterator.toSet))      // using iterator should be faster than obtaining a sorted list
        xNeighborsR.enqueue((xDist, tR))
        yNeighborsR.enqueue((yDist, tR))
      } else {
        if (currentNNR.add(dist)) {
          kNeighborDistsR += ((tR, currentNNR.iterator.toSet))
        }
        if (xDist < currentNNR.max) {                             // since the tR<k was handled explicitly, we are now able to really use a lower-than condition here
          xNeighborsR.enqueue((xDist, tR))
        }
        if (yDist < currentNNR.max) {
          yNeighborsR.enqueue((yDist, tR))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsR.last}%-20s\n * currentNNR: ${currentNNR.sortedList}\n * in-xR-slice: ${xNeighborsR.toList.sorted}\n * in-yR-slice: ${yNeighborsR.toList.sorted}")
      }
      tR += 1
    } else {
      if (currentNNL.length < k) {
        currentNNL.add(dist)
        kNeighborDistsL += ((tL, currentNNL.iterator.toSet))
        xNeighborsL.enqueue((xDist, tL))
        yNeighborsL.enqueue((yDist, tL))
      } else {
        if (currentNNL.add(dist)) {
          kNeighborDistsL += ((tL, currentNNL.iterator.toSet))
        }
        if (xDist < currentNNL.max) {
          xNeighborsL.enqueue((xDist, tL))
        }
        if (yDist < currentNNL.max) {
          yNeighborsL.enqueue((yDist, tL))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsL.last}%-20s\n * currentNNL: ${currentNNL.sortedList}\n * in-xL-slice: ${xNeighborsL.toList.sorted}\n * in-yL-slice: ${yNeighborsL.toList.sorted}")
      }
      tL += 1
    }
  }
  
  def getNeighborhood(nL: Int, nR: Int): (Int, Int, Double, Int, Int) = {
    if (nL+nR < k) {
      // debugOut.println(f"\n * query with nL=$nL nR=$nR   => min k not satisfied")
      return (0, 0, 0.0, 0, 0)
    } else { 
      // first determine the relevant nearest neighbor sets in both time directions
      val kNeighborDistLIndex = BinarySearch.lastIndex_<(kNeighborDistsL.length, nL, i => kNeighborDistsL.apply(i)._1)
      val kNeighborDistRIndex = BinarySearch.lastIndex_<(kNeighborDistsR.length, nR, i => kNeighborDistsR.apply(i)._1)
      val kNeighborDistL = kNeighborDistsL(kNeighborDistLIndex)._2
      val kNeighborDistR = kNeighborDistsR(kNeighborDistRIndex)._2
      // correctness check of binary search:
      //   val kNeighborDistLLinearSearch = kNeighborDistsL.filter(_._1 < nL).last._2
      //   val kNeighborDistRLinearSearch = kNeighborDistsR.filter(_._1 < nR).last._2
      //   assert(kNeighborDistL == kNeighborDistLLinearSearch, f"nn by linear search $kNeighborDistLLinearSearch must equal nn by binary search $kNeighborDistL")
      //   assert(kNeighborDistR == kNeighborDistRLinearSearch, f"nn by linear search $kNeighborDistRLinearSearch must equal nn by binary search $kNeighborDistR")
      // now create union and determine the "combined" kNN
      val kNeighborDist = (kNeighborDistL ++ kNeighborDistR).toList.sorted.apply(k-1)   // TODO: instead of sorting, there are much faster ways to obtain the (k-1) element
      val nxL = xNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nyL = yNeighborsL.toArray.filter(n => n._1 < kNeighborDist && n._2 < nL).length
      val nxR = xNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      val nyR = yNeighborsR.toArray.filter(n => n._1 < kNeighborDist && n._2 < nR).length
      /*
      debugOut.println(f"\n * query with nL=$nL nR=$nR    set of nnL = ${kNeighborDistL}    set of nnR = ${kNeighborDistR}")
      debugOut.println(f" * NN History L: " + kNeighborDistsL)
      debugOut.println(f" * NN History R: " + kNeighborDistsR)
      debugOut.println(" * points in xL " + xNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL))
      debugOut.println(" * points in yL " + yNeighborsL.toList.filter(n => n._1 < kNeighborDist && n._2 < nL))
      debugOut.println(" * points in xR " + xNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR))
      debugOut.println(" * points in yR " + yNeighborsR.toList.filter(n => n._1 < kNeighborDist && n._2 < nR))
      */
      return (nxL+nxR, nyL+nyR, kNeighborDist, tL min nL, tR min nR)
    }
  }
  
  def printInfo() {
    //println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
}
class FactoryNeighborSummary2DIncremental_v3 extends FactoryNeighborSummary2D {
  def build(x: Double, y: Double, k: Int) = new NeighborSummary2DIncremental_v3(x, y, k)
}














