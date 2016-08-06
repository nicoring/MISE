package dmf.stream.mutinf

import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.util.Random
import scala.deprecated

import org.apache.commons.math3.distribution.ExponentialDistribution
import org.apache.commons.math3.special.Gamma.digamma

import dmf.Helpers
import dmf.SortedBufferKeepLow
import dmf.Stopwatch
import dmf.Implicits._





/**
 * MISE implementation v1, properties:
 * - simple "exponential" sampling of query anchors
 * - reverse initialization via separate data buffer
 */
@deprecated("This is not the final MISE implementation.", "long time")
class EstimatorStreamMISEv1(val factory: EstimatorStreamMISEv1Factory, val k: Int, maxQueryAnchors: Int) extends EstimatorStream {

  case class QueryAnchor(pos: Int, x: Double, y: Double, neighbors: NeighborSummary2D)
  
  val queryAnchors = collection.mutable.ArrayBuffer[QueryAnchor]()
  val dataBuffer = collection.mutable.ListBuffer[(Double, Double)]()
  
  var pos = 0
  
  def miQueryImpl(w: WindowSpec): EstimationResult = {
    
    // determine anchors in query window
    val miExpValues = queryAnchors.filter{ anchor => w.indexFilter(anchor.pos) }.map { anchor =>
      val lenL = (anchor.pos - w.indexMin).toInt // TODO: long version necessary? probably not, since differences to window borders should never get that big.
      val lenR = (w.indexMax - anchor.pos).toInt 
      val (nx, ny, kNeighborDist, seenL, seenR) = anchor.neighbors.getNeighborhood(lenL, lenR)
      //println(f"id = ${anchor.pos}%3d    x = ${anchor.x}%8.2f y = ${anchor.y}%8.2f    $nx%4d $ny%4d    neighbor dist:    $kNeighborDist%8.2f    $lenL%4d    $lenR%4d")
      
      val seen = seenL+seenR+1 // windowR-windowL+1
      //println(f"id = ${anchor.pos}%5d    $seenL%5d    $seenR%5d    $seen%5d")
      ( digamma(k) - (digamma(nx+1) + digamma(ny+1)) + digamma(seen) ) / math.log(2) 
    }
    
    //println(f"number of matching query anchors: ${miExpValues.length}")
    
    val mi = miExpValues.sum / miExpValues.length
    EstimationResult(mi, miExpValues.length)
  }
  
  def addData(x: Double, y: Double) {

    // first show the data point to all existing query anchors
    queryAnchors.foreach(anchor => anchor.neighbors.addNeighbor(x, y, true))

    // create new anchor
    val newAnchor = new QueryAnchor(pos, x, y, new NeighborSummary2DIncremental_v2(x, y, k)) // new TrivialNeighborSummary2D(x, y, k)
    
    // reverse initialization
    // queryAnchors.reverse.foreach(anchor => newAnchor.neighbors.addNeighbor(anchor.x, anchor.y, false))
    dataBuffer.reverse.foreach(p => newAnchor.neighbors.addNeighbor(p._1, p._2, false))
    // caution: 
    // super important to reverse iterate over existing query anchors!!!
    // otherwise the closest-to-left is the oldest, which is actually farther-to-left...

    // append new anchor
    queryAnchors += newAnchor
    pos += 1
    
    // delete if maxQueryAnchors is exceeded
    if (queryAnchors.length > maxQueryAnchors) {
      val randInd = Random.nextInt(queryAnchors.length)
      //println(f"deleting anchor at pos ${queryAnchors(randInd).pos}")
      queryAnchors.remove(randInd)
    }
    
    // store data
    dataBuffer += ((x,y))
    if (dataBuffer.length > maxQueryAnchors) {
      dataBuffer.remove(0)
    }
  }
  
  def length = pos

  val referenceImplementation = true
  val minPossibleQuerySize = k+1
  
}

class EstimatorStreamMISEv1Factory(val k: Int, maxQueryAnchors: Int) extends EstimatorStreamFactory {
  def build() = new EstimatorStreamMISEv1(this, k, maxQueryAnchors)
  val estimatorDescription = f"MISE_v1_$maxQueryAnchors"
}



/**
 * MISE implementation v2, properties:
 * - flexible instantiation of reservoir sampling possible; currently: RECIPROCAL
 */
@deprecated("This is not the final MISE implementation.", "long time")
class EstimatorStreamMISEv2(val factory: EstimatorStreamMISEv2Factory, k: Int, deltaMax: Double, maxDataBufferLength: Int) extends EstimatorStream {

  case class QueryAnchor(pos: Int, x: Double, y: Double, neighbors: NeighborSummary2D)
  
  val queryAnchors: Reservoir[QueryAnchor] = new IncrementalReciprocalReservoirSamplingOptimized[QueryAnchor](deltaMax) 
    //collection.mutable.ArrayBuffer[QueryAnchor]()
  val dataBuffer = collection.mutable.ListBuffer[(Double, Double)]()
  
  var pos = 0
  
  def miQueryImpl(w: WindowSpec): EstimationResult = {
    
    val queryAnchorsInWindow = queryAnchors.getReservoir.filter{ case (pos, anchor) => w.indexFilter(anchor.pos) }
    
    // determine anchors in query window
    val miExpValues = queryAnchorsInWindow.map { case (pos, anchor) =>
      val lenL = (anchor.pos - w.indexMin).toInt // TODO: long version necessary? probably not, since differences to window borders should never get that big.
      val lenR = (w.indexMax - anchor.pos).toInt 
      val (nx, ny, kNeighborDist, seenL, seenR) = anchor.neighbors.getNeighborhood(lenL, lenR)
      // println(f"id = ${anchor.pos}%3d    x = ${anchor.x}%8.2f y = ${anchor.y}%8.2f    $nx%4d $ny%4d    neighbor dist:    $kNeighborDist%8.2f    $lenL%4d    $lenR%4d")
      
      val seen = seenL+seenR+1 // windowR-windowL+1
      // println(f"id = ${anchor.pos}%5d    $seenL%5d    $seenR%5d    $seen%5d")
      ( digamma(k) - (digamma(nx+1) + digamma(ny+1)) + digamma(seen) ) / math.log(2) 
    }
    
    // println(f"number of matching query anchors: ${miExpValues.length}%5d    for window $w")
    
    val mi = miExpValues.sum / miExpValues.length
    EstimationResult(mi, miExpValues.length)
  }
  
  def addData(x: Double, y: Double) {

    // first show the data point to all existing query anchors
    queryAnchors.getReservoir.foreach{ case (pos, anchor) => anchor.neighbors.addNeighbor(x, y, true)}

    // create new anchor
    val newAnchor = new QueryAnchor(pos, x, y, new NeighborSummary2DIncremental_v2(x, y, k)) // new TrivialNeighborSummary2D(x, y, k)
    
    // reverse initialization
    // queryAnchors.reverse.foreach(anchor => newAnchor.neighbors.addNeighbor(anchor.x, anchor.y, false))
    dataBuffer.reverse.foreach(p => newAnchor.neighbors.addNeighbor(p._1, p._2, false))
    // caution: 
    // super important to reverse iterate over existing query anchors!!!
    // otherwise the closest-to-left is the oldest, which is actually farther-to-left...

    // append new anchor, handling of deletion is automatically handled by the reservoir as well
    queryAnchors.add(newAnchor)
    pos += 1
    
    // store data
    dataBuffer += ((x,y))
    if (dataBuffer.length > maxDataBufferLength) {
      dataBuffer.remove(0)
    }
  }
  
  def length = pos

  val referenceImplementation = true
  val minPossibleQuerySize = k+1
  
  //override def toString = f"MISE_v2_${deltaMax}_${maxDataBufferLength}"
  
}

class EstimatorStreamMISEv2Factory(k: Int, deltaMax: Double, maxDataBufferLength: Int) extends EstimatorStreamFactory {
  def build() = new EstimatorStreamMISEv2(this, k, deltaMax, maxDataBufferLength)
  val estimatorDescription = f"MISE_v2_${deltaMax}_${maxDataBufferLength}"
}


/**
 * MISE implementation v3, properties:
 * - flexible instantiation of reservoir sampling possible; currently: RECIPROCAL
 * - uses new implementation of QueryAnchors, allowing sparse data filling
 */
class EstimatorStreamMISEv3(val factory: EstimatorStreamMISEv3Factory, k: Int, reservoirFactory: () => Reservoir[QueryAnchor]) extends EstimatorStream {

  //val queryAnchors: Reservoir[QueryAnchor] = new IncrementalReciprocalReservoirSamplingOptimized[QueryAnchor](deltaMax) 
  val queryAnchors = reservoirFactory()
  
  var pos = 0
  
  def addData(x: Double, y: Double) {

    // first show the data point to all existing query anchors
    val existingQueryAnchors = queryAnchors.iterator.toArray 
    existingQueryAnchors.foreach{ case anchor => anchor.addData(x, y, pos) }

    // create new anchor
    val newAnchor = new QueryAnchorIncremental_v2(x, y, pos, k)
    
    // reverse initialization
    existingQueryAnchors.reverse.foreach{ case anchor => newAnchor.addData(anchor.x, anchor.y, anchor.pos) }
    // caution: 
    // super important to reverse iterate over existing query anchors!!!
    // otherwise the closest-to-left is the oldest, which is actually farther-to-left...

    // append new anchor, handling of deletion is automatically handled by the reservoir as well
    queryAnchors.add(newAnchor)
    pos += 1
  }

  def miQueryImpl(w: WindowSpec): EstimationResult = {
    
    // println(this.toString() + " -- query: " + w + "    pos: " + pos)
    val queryAnchorsInWindow = queryAnchors.iterator.filter{ anchor => w.indexFilter(anchor.pos) }.toArray
    
    // determine anchors in query window
    val miExpValues = queryAnchorsInWindow.flatMap(anchor => anchor.getMarginalCounts(w).map(res => (anchor, res))).map { case (anchor, queryRes) =>

      val seen = queryRes.seenL + queryRes.seenR + 1
      //val seen = w.size
      //val seen = anchor.maxSeenR - anchor.maxSeenL
      
      val estimate = ( digamma(k) - (digamma(queryRes.nx+1) + digamma(queryRes.ny+1)) + digamma(seen) ) / math.log(2) 
      // println(f"id = ${anchor.pos}%3d    x = ${anchor.x}%8.2f y = ${anchor.y}%8.2f    ${queryRes.nx}%4d ${queryRes.ny}%4d    neighbor dist:    ${queryRes.kNeighborDist}%8.2f    ${queryRes.seenL}%4d    ${queryRes.seenR}%4d    $estimate%6.3f")
      estimate
    }
    
    // println(f"number of matching query anchors: ${miExpValues.length}%5d    for window $w")
    
    val mi = miExpValues.sum / miExpValues.length
    EstimationResult(mi, miExpValues.length)
  }
  
  def length = pos

  val referenceImplementation = true
  val minPossibleQuerySize = k+1

  def writeDebugInfo() {
    val out = Helpers.outputFile("query_anchor_distribution.csv")
    queryAnchors.iterator.foreach{ anchor =>
      val min = anchor.maxSeenL
      val max = anchor.maxSeenR
      val seen = Range(min.toInt, max.toInt+1).filter(pos => anchor.hasSeen(pos)).toList
      out.println((anchor.pos :: min :: max :: seen).mkString(";"))
    }
    out.close()
  }
}

class EstimatorStreamMISEv3Factory(k: Int, sampling: String, param: Int) extends EstimatorStreamFactory {
  def build() = {
    val factory = sampling match {
      case "RS" => () => new TraditionalReservoirSampling[QueryAnchor](param)
      case "SW" => () => new SlidingWindowSampling[QueryAnchor](param)
      case "MS_F" => () => new IncrementalReciprocalReservoirSamplingFixedSample[QueryAnchor](param)
      case "MS_D" => () => new IncrementalReciprocalReservoirSamplingOptimized[QueryAnchor](param)
    }
    new EstimatorStreamMISEv3(this, k, factory)
  }
  val estimatorDescription = f"MISE_v3_${sampling}_$param"
}





/**
 * OLD APPROACH 1:
 * The idea behind this was that the reverse initialization is performed "in a batch".
 * Unclear whether this will be needed...
 * Implementation of QueryAnchor was in every case unfinished...
class QueryAnchor(val pos: Int, val sampleLengthL: Int, val sampleLengthR: Int, val x: Double, val y: Double, k: Int) {
  
  val windowL = pos - sampleLengthL
  val windowR = pos + sampleLengthR
  
  def initNeighborhood(data: Array[(Double, Double)]) {
    val distances = data.map{ case (x,y) => x max y }
    val minDist = distances.min
  }
  
  def updateNeighborhood(neighborPos: Int, neighborX: Double, neighborY: Double) {
    
  }
}

class KraskovStreamEstimator(val maxQueryAnchors: Int, k: Int) extends MIEstimatorVariableQueries {

  var len = 0
  val randExp = new ExponentialDistribution(10.0)
  val queryAnchors = collection.mutable.ListBuffer[QueryAnchor]()
  
  def calcMI(windowL: Int, windowR: Int): Double = {
    assert(windowL >= 0 && windowR < len && windowR-windowL>0)
    ???
  }
  
  def addData(x: Double, y: Double) {

    val sampleLengthL = randExp.sample().toInt
    val sampleLengthR = randExp.sample().toInt
    val newAnchor = new QueryAnchor(len, sampleLengthL, sampleLengthR, x, y, k)
    
    // reverse initialization
    val initData = collection.mutable.ArrayBuffer[(Double, Double)]()
    for (anchor <- queryAnchors) {
      if (anchor.pos >= newAnchor.windowL) {
        //newAnchor.updateNeighborhood(anchor.pos, anchor.x, anchor.y)
        initData += ((anchor.x, anchor.y))
      }
    }
    newAnchor.initNeighborhood(initData.toArray)
    
    queryAnchors += newAnchor
    
    // remove anchors if necessary
    if (queryAnchors.length > maxQueryAnchors) {
      val removedAnchor = queryAnchors.remove(Random.nextInt(maxQueryAnchors))
    }
    
    len += 1
  }
  
  def length = len
  
}
*/


object DummyToAvoidEclipseBugs

