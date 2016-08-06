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

case class MarginalCountsQueryResult(nx: Int, ny: Int, kNeighborDist: Double, seenL: Int, seenR: Int)

trait QueryAnchor {
  val x: Double
  val y: Double
  val pos: Long
  def addData(dataX: Double, dataY: Double, dataPos: Long)
  def getMarginalCounts(w: WindowSpec): Option[MarginalCountsQueryResult]
  
  // functions to externally retrieve which information is stored in a query anchor
  def maxSeenL: Long
  def maxSeenR: Long
  def hasSeen(p: Long): Boolean
}

trait FactoryQueryAnchor {
  def build(x: Double, y: Double, pos: Long, k: Int): QueryAnchor
  val description: String = this.getClass.getSimpleName
  override def toString() = description 
}


/**
 * Trivial implementation: build neighborhood for each query
 */
class QueryAnchorReference(val x: Double, val y: Double, val pos: Long, k: Int) extends QueryAnchor {
  
  val dataL = new collection.mutable.ArrayBuffer[(Long, Double, Double)]()
  val dataR = new collection.mutable.ArrayBuffer[(Long, Double, Double)]()
  
  var maxPosL = pos
  var maxPosR = pos
  
  def addData(dataX: Double, dataY: Double, dataPos: Long) {
    assert(dataPos != pos, f"query anchor at position $dataPos received a data element with identical position")
    assert(dataPos < maxPosL || dataPos > maxPosR, f"new position must either be lower or higher: $dataPos did not extend current interval [$maxPosL, $maxPosR]")
    if (dataPos > maxPosR) {
      dataR += ((dataPos, x-dataX, y-dataY))    // note: relative positions are stored
      maxPosR = dataPos
    } else {
      dataL += ((dataPos, x-dataX, y-dataY))
      maxPosL = dataPos
    }
  }
  
  def getMarginalCounts(w: WindowSpec): Option[MarginalCountsQueryResult] = {
    if (w.size <= k)                        // if the  window is not big enough; size must really be larger than k, since the window contains the query anchor itself
      return None
    else if (!w.indexFilter(pos))           // if the window does not contain the query anchor
      return None
    else {
      val sampleL = dataL.filter(e => w.indexFilter(e._1))
      val sampleR = dataR.filter(e => w.indexFilter(e._1))
      val sample = sampleL ++ sampleR
      if (sample.length < k) {
        // due to sparse data, it is still possible that we do not reach k objects in total, even if the window size would allow that
        return None
      }
      val distances = sample.map(p => math.abs(p._2) max math.abs(p._3)).sorted     // positions are already relative, no subtraction from x/y necessary
      val kNeighborDist = distances(k-1)
      val nx = sample.filter(p => math.abs(p._2) < kNeighborDist).length
      val ny = sample.filter(p => math.abs(p._3) < kNeighborDist).length
      //println(f"kNeighborDist = $kNeighborDist%10.6f")
      return Some(MarginalCountsQueryResult(nx, ny, kNeighborDist, sampleL.length, sampleR.length))
    }
  }
  
  def maxSeenL = maxPosL
  def maxSeenR = maxPosR
  def hasSeen(p: Long) = (dataL.map(_._1).toSet ++ dataR.map(_._1).toSet).contains(p)
}
class FactoryQueryAnchorReference extends FactoryQueryAnchor {
  def build(x: Double, y: Double, pos: Long, k: Int) = new QueryAnchorReference(x, y, pos, k)
}


/**
 * An optimized version of the reference implementation:
 * - insert is the same
 * - in the query: takeWhile instead of filter => O(WindowSize) instead of O(StreamSize)
 */
class QueryAnchorReferenceOptimized(val x: Double, val y: Double, val pos: Long, k: Int) extends QueryAnchor {
  
  val dataL = new collection.mutable.ArrayBuffer[(Long, Double, Double)]()
  val dataR = new collection.mutable.ArrayBuffer[(Long, Double, Double)]()
  
  var maxPosL = pos
  var maxPosR = pos
  
  def addData(dataX: Double, dataY: Double, dataPos: Long) {
    assert(dataPos != pos, f"query anchor at position $dataPos received a data element with identical position")
    assert(dataPos < maxPosL || dataPos > maxPosR, f"new position must either be lower or higher: $dataPos did not extend current interval [$maxPosL, $maxPosR]")
    if (dataPos > maxPosR) {
      dataR += ((dataPos, x-dataX, y-dataY))    // note: relative positions are stored
      maxPosR = dataPos
    } else {
      dataL += ((dataPos, x-dataX, y-dataY))
      maxPosL = dataPos
    }
  }
  
  def getMarginalCounts(w: WindowSpec): Option[MarginalCountsQueryResult] = {
    if (w.size <= k)                        // if the  window is not big enough; size must really be larger than k, since the window contains the query anchor itself
      return None
    else if (!w.indexFilter(pos))           // if the window does not contain the query anchor
      return None
    else {
      val sampleL = dataL.takeWhile(e => w.indexFilter(e._1))
      val sampleR = dataR.takeWhile(e => w.indexFilter(e._1))
      val sample = sampleL ++ sampleR
      if (sample.length < k) {
        // due to sparse data, it is still possible that we do not reach k objects in total, even if the window size would allow that
        return None
      }
      val distances = sample.map(p => math.abs(p._2) max math.abs(p._3)).sorted     // positions are already relative, no subtraction from x/y necessary
      val kNeighborDist = distances(k-1)
      val nx = sample.filter(p => math.abs(p._2) < kNeighborDist).length
      val ny = sample.filter(p => math.abs(p._3) < kNeighborDist).length
      //println(f"kNeighborDist = $kNeighborDist%10.6f")
      return Some(MarginalCountsQueryResult(nx, ny, kNeighborDist, sampleL.length, sampleR.length))
    }
  }
  
  def maxSeenL = maxPosL
  def maxSeenR = maxPosR
  def hasSeen(p: Long) = (dataL.map(_._1).toSet ++ dataR.map(_._1).toSet).contains(p)
}
class FactoryQueryAnchorReferenceOptimized extends FactoryQueryAnchor {
  def build(x: Double, y: Double, pos: Long, k: Int) = new QueryAnchorReferenceOptimized(x, y, pos, k)
}



/**
 * Incremental Version 1:
 * Based on version 3 of NeighborSummary2DIncremental
 */
class QueryAnchorIncremental_v1(val x: Double, val y: Double, val pos: Long, k: Int) extends QueryAnchor {
  
  // val debugOut = Helpers.outputFile("qa_debug.log")
  
  // storing of "marginal points"
  val xNeighborsL = new collection.mutable.PriorityQueue[(Double, Long)]()
  val xNeighborsR = new collection.mutable.PriorityQueue[(Double, Long)]()
  val yNeighborsL = new collection.mutable.PriorityQueue[(Double, Long)]()
  val yNeighborsR = new collection.mutable.PriorityQueue[(Double, Long)]()

  // in order to determine the "seen-points-in-window": When a data point does not fall into either the x or y marginal we need a way to know that we in fact have seen it...
  val seenPosL = new collection.mutable.ArrayBuffer[Long]()
  val seenPosR = new collection.mutable.ArrayBuffer[Long]()
  
  // the current nearest neighbors 
  val currentNNL = new SortedBufferKeepLow[Double](k) // note: Java implementation is slightly slower for k=1, no: but runtime has much larger variance
  val currentNNR = new SortedBufferKeepLow[Double](k)
  // to store the NN history
  val kNeighborDistsR = new collection.mutable.ArrayBuffer[(Long, Set[Double])]() // initialSize=128    changing initialSize (from default 16) does not seem to have a big effect
  val kNeighborDistsL = new collection.mutable.ArrayBuffer[(Long, Set[Double])]()
  kNeighborDistsR += ((-1, Set()))
  kNeighborDistsL += ((-1, Set()))
  
  var maxPosL = pos
  var maxPosR = pos
  
  /**
   * The performance of an insert is determined by:
   * - insert of new point to current NN, implemented by Java Priority Queue => good performance, TODO: an optimized k=1 implementation
   * - if insert of new point to current NN is successful, store in "NN history" => currently ArrayBuffer => excellent performance
   * - if marginal within slice (max distance should be obtained in O(1)): add to "marginal points" => currently Scala priority queue => not too much to gain, on TODO for queries anyways...
   */
  def addData(dataX: Double, dataY: Double, dataPos: Long) {
    assert(dataPos != pos, f"query anchor at position $dataPos received a data element with identical position")
    assert(dataPos < maxPosL || dataPos > maxPosR, f"new position must either be lower or higher: $dataPos did not extend current interval [$maxPosL, $maxPosR]")
    val xDist = math.abs(x-dataX)
    val yDist = math.abs(y-dataY)
    val dist =  xDist max yDist
    if (dataPos > maxPosR) {
      if (currentNNR.length < k) {
        /**
         * If we do not yet have k neighbors we can get rid of all conditions, since we have to store everything anyway
         * See previous implementation for an explanation (where this was solved by a lower-or-equal condition)
         */
        currentNNR.add(dist)
        kNeighborDistsR += ((dataPos-pos-1, currentNNR.iterator.toSet))      // using iterator should be faster than obtaining a sorted list
        xNeighborsR.enqueue((xDist, dataPos))
        yNeighborsR.enqueue((yDist, dataPos))
      } else {
        if (currentNNR.add(dist)) {
          kNeighborDistsR += ((dataPos-pos-1, currentNNR.iterator.toSet))
        }
        if (xDist < currentNNR.max) {                             // since the tR<k was handled explicitly, we are now able to really use a lower-than condition here
          xNeighborsR.enqueue((xDist, dataPos))
        }
        if (yDist < currentNNR.max) {
          yNeighborsR.enqueue((yDist, dataPos))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsR.last}%-20s\n * currentNNR: ${currentNNR.sortedList}\n * in-xR-slice: ${xNeighborsR.toList.sorted}\n * in-yR-slice: ${yNeighborsR.toList.sorted}")
      }
      seenPosR += dataPos
      maxPosR = dataPos
    } else {
      if (currentNNL.length < k) {
        currentNNL.add(dist)
        kNeighborDistsL += ((pos-dataPos-1, currentNNL.iterator.toSet))
        xNeighborsL.enqueue((xDist, dataPos))
        yNeighborsL.enqueue((yDist, dataPos))
      } else {
        if (currentNNL.add(dist)) {
          kNeighborDistsL += ((pos-dataPos-1, currentNNL.iterator.toSet))
        }
        if (xDist < currentNNL.max) {
          xNeighborsL.enqueue((xDist, dataPos))
        }
        if (yDist < currentNNL.max) {
          yNeighborsL.enqueue((yDist, dataPos))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsL.last}%-20s\n * currentNNL: ${currentNNL.sortedList}\n * in-xL-slice: ${xNeighborsL.toList.sorted}\n * in-yL-slice: ${yNeighborsL.toList.sorted}")
      }
      seenPosL += dataPos
      maxPosL = dataPos
    }
  }
  
  def getMarginalCounts(w: WindowSpec): Option[MarginalCountsQueryResult] = {
    if (w.size <= k)                        // if the  window is not big enough; size must really be larger than k, since the window contains the query anchor itself
      return None
    else if (!w.indexFilter(pos))           // if the window does not contain the query anchor
      return None
    else { 
      // first determine the relevant nearest neighbor sets in both time directions
      val numTakeR = w.indexMax - pos
      val numTakeL = pos - w.indexMin
      val kNeighborDistRIndex = BinarySearch.lastIndex_<(kNeighborDistsR.length, numTakeR, i => kNeighborDistsR.apply(i)._1)
      val kNeighborDistLIndex = BinarySearch.lastIndex_<(kNeighborDistsL.length, numTakeL, i => kNeighborDistsL.apply(i)._1)
      val kNeighborDistR = kNeighborDistsR(kNeighborDistRIndex)._2
      val kNeighborDistL = kNeighborDistsL(kNeighborDistLIndex)._2
      // correctness check of binary search:
      //   val kNeighborDistLLinearSearch = kNeighborDistsL.filter(_._1 < nL).last._2
      //   val kNeighborDistRLinearSearch = kNeighborDistsR.filter(_._1 < nR).last._2
      //   assert(kNeighborDistL == kNeighborDistLLinearSearch, f"nn by linear search $kNeighborDistLLinearSearch must equal nn by binary search $kNeighborDistL")
      //   assert(kNeighborDistR == kNeighborDistRLinearSearch, f"nn by linear search $kNeighborDistRLinearSearch must equal nn by binary search $kNeighborDistR")

      // now create union of both sides and determine the "combined" kNN
      val joinedNeighbors = kNeighborDistL ++ kNeighborDistR
      if (joinedNeighbors.size < k) {
        return None
      }
      val kNeighborDist = joinedNeighbors.toList.sorted.apply(k-1)   // TODO: instead of sorting, there are much faster ways to obtain the k-th element, but only interesting if k is large, since total number is just 2*k
      val nxR = xNeighborsR.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val nyR = yNeighborsR.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val nxL = xNeighborsL.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val nyL = yNeighborsL.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      
      /*
      debugOut.println(f"\n * query with nL=$numTakeL nR=$numTakeR    set of nnL = ${kNeighborDistL}    set of nnR = ${kNeighborDistR}")
      debugOut.println(f" * NN History L: " + kNeighborDistsL)
      debugOut.println(f" * NN History R: " + kNeighborDistsR)
      debugOut.println(" * points in xL " + xNeighborsL.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in yL " + yNeighborsL.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in xR " + xNeighborsR.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in yR " + yNeighborsR.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      */
      
      // Determination of how many points were seen within the window:
      // a trivial implementation would simply filter seenPosR and seenPosL according to which positions are within the query window (see below)
      // however, this would case a complexity linear with the query window size (was clearly visible as a linear runtime increase in MISE)
      // instead we use a binary search:
      // for the R-side we search for the last position that is still in the window (last <= max index).
      // for the L-side we have to invert the ordering, since binary search requires increasing order; inverting the ordering allows to apply exactly the same logic
      // in general BineraySearch.lastIndex returns an index, so the question is: Is it necessary to convert the index again to a position and subtract this.pos?
      // no! the index directly corresponds to the number of elements within the window, we simply have to add 1, since the index found is within the window.
      // and do we have to catch the no-index-found case?
      // no! in this case the returned index is -1 by convention. Adding 1 gives the correct result of "zero elements seen".
      val seenR = 1 + BinarySearch.lastIndex_<=(seenPosR.length, w.indexMax, i => seenPosR.apply(i))
      val seenL = 1 + BinarySearch.lastIndex_<=(seenPosL.length, w.indexMin, i => seenPosL.apply(i))(Ordering.Long.reverse)
      /* the reference implementation:
      val seenRCorr = seenPosR.filter(w.indexFilter(_)).length
      val seenLCorr = seenPosL.filter(w.indexFilter(_)).length
      assert(seenR == seenRCorr, f"$seenR != $seenRCorr    seenPosR: $seenPosR    pos: $pos    ${w.indexMax}")
      assert(seenL == seenLCorr, f"$seenL != $seenLCorr    seenPosR: $seenPosR    pos: $pos    ${w.indexMin}")
      */
      return Some(MarginalCountsQueryResult(nxL+nxR, nyL+nyR, kNeighborDist, seenL.toInt, seenR.toInt))
    }
  }
  
  def maxSeenL = maxPosL
  def maxSeenR = maxPosR
  def hasSeen(p: Long) = seenPosL.contains(p) || seenPosR.contains(p)

  def printInfo() {
    //println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
}
class FactoryQueryAnchorIncremental_v1 extends FactoryQueryAnchor {
  def build(x: Double, y: Double, pos: Long, k: Int) = new QueryAnchorIncremental_v1(x, y, pos, k)
}



/**
 * Incremental Version 2:
 * - query complexity: no longer O(StreamLength) 
 */
class QueryAnchorIncremental_v2(val x: Double, val y: Double, val pos: Long, k: Int) extends QueryAnchor {
  
  // val debugOut = Helpers.outputFile("qa_debug.log")
  
  // storing of "marginal points"
  val xNeighborsL = new collection.mutable.ArrayBuffer[(Double, Long)]() // initialSize=128)
  val xNeighborsR = new collection.mutable.ArrayBuffer[(Double, Long)]() // initialSize=128)
  val yNeighborsL = new collection.mutable.ArrayBuffer[(Double, Long)]() // initialSize=128)
  val yNeighborsR = new collection.mutable.ArrayBuffer[(Double, Long)]() // initialSize=128)

  // in order to determine the "seen-points-in-window": When a data point does not fall into either the x or y marginal we need a way to know that we in fact have seen it...
  val seenPosL = new collection.mutable.ArrayBuffer[Long]()
  val seenPosR = new collection.mutable.ArrayBuffer[Long]()
  
  // the current nearest neighbors 
  val currentNNL = new SortedBufferKeepLow[Double](k) // note: Java implementation is slightly slower for k=1, no: but runtime has much larger variance
  val currentNNR = new SortedBufferKeepLow[Double](k)
  // to store the NN history
  val kNeighborDistsR = new collection.mutable.ArrayBuffer[(Long, Set[Double])]() // initialSize=128    changing initialSize (from default 16) does not seem to have a big effect
  val kNeighborDistsL = new collection.mutable.ArrayBuffer[(Long, Set[Double])]()
  kNeighborDistsR += ((-1, Set()))
  kNeighborDistsL += ((-1, Set()))
  
  var maxPosL = pos
  var maxPosR = pos
  
  /**
   * The performance of an insert is determined by:
   * - insert of new point to current NN, implemented by Java Priority Queue => good performance, TODO: an optimized k=1 implementation
   * - if insert of new point to current NN is successful, store in "NN history" => currently ArrayBuffer => excellent performance
   * - if marginal within slice (max distance should be obtained in O(1)): add to "marginal points" => currently Scala priority queue => not too much to gain, on TODO for queries anyways...
   */
  def addData(dataX: Double, dataY: Double, dataPos: Long) {
    assert(dataPos != pos, f"query anchor at position $dataPos received a data element with identical position")
    assert(dataPos < maxPosL || dataPos > maxPosR, f"new position must either be lower or higher: $dataPos did not extend current interval [$maxPosL, $maxPosR]")
    val xDist = math.abs(x-dataX)
    val yDist = math.abs(y-dataY)
    val dist =  xDist max yDist
    if (dataPos > maxPosR) {
      if (currentNNR.length < k) {
        /**
         * If we do not yet have k neighbors we can get rid of all conditions, since we have to store everything anyway
         * See previous implementation for an explanation (where this was solved by a lower-or-equal condition)
         */
        currentNNR.add(dist)
        kNeighborDistsR += ((dataPos-pos-1, currentNNR.iterator.toSet))      // using iterator should be faster than obtaining a sorted list
        xNeighborsR += ((xDist, dataPos))
        yNeighborsR += ((yDist, dataPos))
      } else {
        if (currentNNR.add(dist)) {
          kNeighborDistsR += ((dataPos-pos-1, currentNNR.iterator.toSet))
        }
        if (xDist < currentNNR.max) {                             // since the tR<k was handled explicitly, we are now able to really use a lower-than condition here
          xNeighborsR += ((xDist, dataPos))
        }
        if (yDist < currentNNR.max) {
          yNeighborsR += ((yDist, dataPos))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsR.last}%-20s\n * currentNNR: ${currentNNR.sortedList}\n * in-xR-slice: ${xNeighborsR.toList.sorted}\n * in-yR-slice: ${yNeighborsR.toList.sorted}")
      }
      seenPosR += dataPos
      maxPosR = dataPos
    } else {
      if (currentNNL.length < k) {
        currentNNL.add(dist)
        kNeighborDistsL += ((pos-dataPos-1, currentNNL.iterator.toSet))
        xNeighborsL += ((xDist, dataPos))
        yNeighborsL += ((yDist, dataPos))
      } else {
        if (currentNNL.add(dist)) {
          kNeighborDistsL += ((pos-dataPos-1, currentNNL.iterator.toSet))
        }
        if (xDist < currentNNL.max) {
          xNeighborsL += ((xDist, dataPos))
        }
        if (yDist < currentNNL.max) {
          yNeighborsL += ((yDist, dataPos))
        }
        // debugOut.println(f"\n * after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDistsL.last}%-20s\n * currentNNL: ${currentNNL.sortedList}\n * in-xL-slice: ${xNeighborsL.toList.sorted}\n * in-yL-slice: ${yNeighborsL.toList.sorted}")
      }
      seenPosL += dataPos
      maxPosL = dataPos
    }
  }
  
  def getMarginalCounts(w: WindowSpec): Option[MarginalCountsQueryResult] = {
    if (w.size <= k)                        // if the  window is not big enough; size must really be larger than k, since the window contains the query anchor itself
      return None
    else if (!w.indexFilter(pos))           // if the window does not contain the query anchor
      return None
    else { 
      // first determine the relevant nearest neighbor sets in both time directions
      val numTakeR = w.indexMax - pos
      val numTakeL = pos - w.indexMin
      val kNeighborDistRIndex = BinarySearch.lastIndex_<(kNeighborDistsR.length, numTakeR, i => kNeighborDistsR.apply(i)._1)
      val kNeighborDistLIndex = BinarySearch.lastIndex_<(kNeighborDistsL.length, numTakeL, i => kNeighborDistsL.apply(i)._1)
      val kNeighborDistR = kNeighborDistsR(kNeighborDistRIndex)._2
      val kNeighborDistL = kNeighborDistsL(kNeighborDistLIndex)._2
      // correctness check of binary search:
      //   val kNeighborDistLLinearSearch = kNeighborDistsL.filter(_._1 < nL).last._2
      //   val kNeighborDistRLinearSearch = kNeighborDistsR.filter(_._1 < nR).last._2
      //   assert(kNeighborDistL == kNeighborDistLLinearSearch, f"nn by linear search $kNeighborDistLLinearSearch must equal nn by binary search $kNeighborDistL")
      //   assert(kNeighborDistR == kNeighborDistRLinearSearch, f"nn by linear search $kNeighborDistRLinearSearch must equal nn by binary search $kNeighborDistR")

      // now create union of both sides and determine the "combined" kNN
      val joinedNeighbors = kNeighborDistL ++ kNeighborDistR
      if (joinedNeighbors.size < k) {
        return None
      }
      val kNeighborDist = joinedNeighbors.toList.sorted.apply(k-1)   // TODO: instead of sorting, there are much faster ways to obtain the k-th element, but only interesting if k is large, since total number is just 2*k
      
      // now determine "marginal points" which are both (1) in the window and (2) have a lower marginal distance than the kNN
      // since the (probably) more restrictive constraint is (1) and by the nature of the stream we already have a sorted array, we can apply binary search
      val xrMaxIndexInWindow = BinarySearch.lastIndex_<=(xNeighborsR.length, w.indexMax, i => xNeighborsR.apply(i)._2)
      val yrMaxIndexInWindow = BinarySearch.lastIndex_<=(yNeighborsR.length, w.indexMax, i => yNeighborsR.apply(i)._2)
      val xlMaxIndexInWindow = BinarySearch.lastIndex_<=(xNeighborsL.length, w.indexMin, i => xNeighborsL.apply(i)._2)(Ordering.Long.reverse)
      val ylMaxIndexInWindow = BinarySearch.lastIndex_<=(yNeighborsL.length, w.indexMin, i => yNeighborsL.apply(i)._2)(Ordering.Long.reverse)
      // now, we only have to apply the filtering for "distance < kNN" for the remaining sub-arrays
      val nxR = if (xrMaxIndexInWindow == -1) 0 else xNeighborsR.take(xrMaxIndexInWindow+1).filter(n => n._1 < kNeighborDist).length
      val nyR = if (yrMaxIndexInWindow == -1) 0 else yNeighborsR.take(yrMaxIndexInWindow+1).filter(n => n._1 < kNeighborDist).length
      val nxL = if (xlMaxIndexInWindow == -1) 0 else xNeighborsL.take(xlMaxIndexInWindow+1).filter(n => n._1 < kNeighborDist).length
      val nyL = if (ylMaxIndexInWindow == -1) 0 else yNeighborsL.take(ylMaxIndexInWindow+1).filter(n => n._1 < kNeighborDist).length    // this was causing java.lang.OutOfMemoryError: GC overhead limit exceeded
      /* reference implementation:
      val correct_nxR = xNeighborsR.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val correct_nyR = yNeighborsR.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val correct_nxL = xNeighborsL.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      val correct_nyL = yNeighborsL.toArray.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)).length
      assert(nxR == correct_nxR, f"$nxR != $correct_nxR")
      assert(nyR == correct_nyR, f"$nyR != $correct_nyR")
      assert(nxL == correct_nxL, f"$nxL != $correct_nxL")
      assert(nyL == correct_nyL, f"$nyL != $correct_nyL")
      */
      
      /*
      debugOut.println(f"\n * query with nL=$numTakeL nR=$numTakeR    set of nnL = ${kNeighborDistL}    set of nnR = ${kNeighborDistR}")
      debugOut.println(f" * NN History L: " + kNeighborDistsL)
      debugOut.println(f" * NN History R: " + kNeighborDistsR)
      debugOut.println(" * points in xL " + xNeighborsL.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in yL " + yNeighborsL.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in xR " + xNeighborsR.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      debugOut.println(" * points in yR " + yNeighborsR.toList.filter(n => n._1 < kNeighborDist && w.indexFilter(n._2)))
      */
      
      // Determination of how many points were seen within the window:
      // a trivial implementation would simply filter seenPosR and seenPosL according to which positions are within the query window (see below)
      // however, this would case a complexity linear with the query window size (was clearly visible as a linear runtime increase in MISE)
      // instead we use a binary search:
      // for the R-side we search for the last position that is still in the window (last <= max index).
      // for the L-side we have to invert the ordering, since binary search requires increasing order; inverting the ordering allows to apply exactly the same logic
      // in general BineraySearch.lastIndex returns an index, so the question is: Is it necessary to convert the index again to a position and subtract this.pos?
      // no! the index directly corresponds to the number of elements within the window, we simply have to add 1, since the index found is within the window.
      // and do we have to catch the no-index-found case?
      // no! in this case the returned index is -1 by convention. Adding 1 gives the correct result of "zero elements seen".
      val seenR = 1 + BinarySearch.lastIndex_<=(seenPosR.length, w.indexMax, i => seenPosR.apply(i))
      val seenL = 1 + BinarySearch.lastIndex_<=(seenPosL.length, w.indexMin, i => seenPosL.apply(i))(Ordering.Long.reverse)
      /* the reference implementation:
      val seenRCorr = seenPosR.filter(w.indexFilter(_)).length
      val seenLCorr = seenPosL.filter(w.indexFilter(_)).length
      assert(seenR == seenRCorr, f"$seenR != $seenRCorr    seenPosR: $seenPosR    pos: $pos    ${w.indexMax}")
      assert(seenL == seenLCorr, f"$seenL != $seenLCorr    seenPosR: $seenPosR    pos: $pos    ${w.indexMin}")
      */
      return Some(MarginalCountsQueryResult(nxL+nxR, nyL+nyR, kNeighborDist, seenL.toInt, seenR.toInt))
    }
  }
  
  def maxSeenL = maxPosL
  def maxSeenR = maxPosR
  def hasSeen(p: Long) = seenPosL.contains(p) || seenPosR.contains(p)

  def printInfo() {
    //println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
}
class FactoryQueryAnchorIncremental_v2 extends FactoryQueryAnchor {
  def build(x: Double, y: Double, pos: Long, k: Int) = new QueryAnchorIncremental_v2(x, y, pos, k)
}










