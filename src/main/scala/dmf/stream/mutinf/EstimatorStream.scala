package dmf.stream.mutinf

import org.apache.commons.math3.special.Gamma.digamma


/**
 * WindowSpec provides an abstraction to specify windows.
 * The general idea to specify windows is to use a "drop take semantic" over a "index semantic", because:
 * - drop take semantic is totally intuitive and no convention is required
 * - drop take semantic immediately gives the window sizes by numTake
 * - drop take semantic immediately allows to check the required stream length by numDrop+numTake
 * - index semantic requires to know the convention of the whole stream is index by 0 or 1 or something else
 * - index semantic requires to know the convention whether the LEFT index should be inclusive or not
 * - index semantic requires to know the convention whether the RIGHT index should be inclusive or not
 * - index semantic allows windows of negative length (max < min)
 * WindowSpec allows to convert to left/right (indexMin, indexMax) indices internally.
 * The semantic of indexMin/indexMax is that 
 * - indexing of whole stream starts at 0
 * - they are both inclusive, i.e., indexMin == indexMax corresponds to a window of size 1
 */
class WindowSpec(val numDrop: Long, val numTake: Long) {
  /** note semantics of indexMin and indexMax are only defined for numTake >= 1 */
  val indexMin = numDrop
  val indexMax = numDrop + numTake - 1
  val size = numTake
  
  def indices(): Iterator[Long] = 
    Iterator.iterate(numDrop)(_+1L).takeWhile(_ <= indexMax)
  
  def indexFilter(i: Long): Boolean = 
    (size > 0 && i >= indexMin && i <= indexMax) 
  
  override def toString() = f"WindowSpec($numDrop, $numTake, $indexMin, $indexMax)"
}
/**
 * Companion to provide both:
 * - construction by "drop take semantic"
 * - construction by "index semantic"
 */
object WindowSpec {
  def apply(numDrop: Long, numTake: Long) = {
    new WindowSpec(numDrop, numTake)
  }
  def createFromIndices(indMin: Long, indMax: Long) = {
    assert(indMax >= indMin)
    new WindowSpec(indMin, indMax - indMin + 1)
  }
  def createReverse(streamLength: Long, numDrop: Long, numTake: Long) = {
    assert(streamLength >= numDrop+numTake)
    new WindowSpec(streamLength-numDrop-numTake, numTake)
  }
}



/**
 * EstimatorStream
 * Convention for streams: indexing starts at 0
 */
trait EstimatorStream {

  case class EstimationResult(mi: Double, statSize: Long)
  
  def addData(x: Double, y: Double)
  def length: Long
  
  /** external implementation of queryMI, which first applies a sanity check, and then calls the abstract implementation
   *  returns:
   *  - mutual information
   *  - size of statistic that was used to perform the estimation
   */
  def miQuery(w: WindowSpec): EstimationResult = {
    assert(w.numDrop + w.numTake <= length && w.numTake >= minPossibleQuerySize)
    miQueryImpl(w)
  }
  protected def miQueryImpl(w: WindowSpec): EstimationResult
  
  /** this is used to indicate whether the estimator should serve as reference */
  val referenceImplementation: Boolean
  /** some estimators have a minimum for the possible window size, e.g. for a large k-NN value */
  val minPossibleQuerySize: Int
  
  /** ensure meaningful toString, linked to Factory */
  val factory: EstimatorStreamFactory
  override def toString() = factory.estimatorDescription 
}

trait EstimatorStreamFactory {
  def build(): EstimatorStream
  val estimatorDescription: String
  override def toString() = "Factory" + estimatorDescription 
}


/**
 * 1. BASELINE:
 * A trivial "stream" estimator that buffers the whole data and 
 * applies a static estimator to the given query window.
 * The provides a best-case baseline
 * - fastest inserts
 * - slowest queries
 * - perfect results for the given estimator (no approximation) 
 */
class EstimatorStreamReference(estimator: EstimatorStatic, val factory: EstimatorStreamReferenceFactory) extends EstimatorStream {

  var len = 0
  val data = collection.mutable.ArrayBuffer[(Double, Double)]()
  
  def miQueryImpl(w: WindowSpec): EstimationResult = {
    val window = data.toArray.drop(w.numDrop.toInt).take(w.numTake.toInt)     // TODO: make "long" version; maybe not necessary because the performance would be terrible anyway...
    val Array(dataX, dataY) = window.map(t => Array(t._1, t._2)).transpose
    val mi = estimator.calcMI(dataX, dataY)
    EstimationResult(mi, w.size)
  }
  
  def addData(x: Double, y: Double) {
    data.append((x,y))
    len += 1
  }
  
  def length = len

  val referenceImplementation = true
  val minPossibleQuerySize = estimator.minPossibleQuerySize
  
}
class EstimatorStreamReferenceFactory(estimator: EstimatorStatic) extends EstimatorStreamFactory {
  def build() = new EstimatorStreamReference(estimator, this)
  val estimatorDescription = "ReferenceEstimator"
}


/**
 * 2. BASELINE:
 * The exact incremental kraskov estimator, or MISE_Baseline.
 * Properties:
 * - uses every point as query anchor
 * - never deletes a point
 * - uses complete reverse initialization
 */
class EstimatorStreamReferenceIncremental(val k: Int, val factory: EstimatorStreamReferenceIncrementalFactory) extends EstimatorStream {

  case class QueryAnchor(pos: Int, x: Double, y: Double, neighbors: NeighborSummary2D)
  
  val queryAnchors = collection.mutable.ArrayBuffer[QueryAnchor]()
  
  def miQueryImpl(w: WindowSpec): EstimationResult = {
    
    // determine anchors in query window
    val expValues = queryAnchors.filter{ anchor => w.indexFilter(anchor.pos) }.map { anchor =>
      val lenL = (anchor.pos - w.indexMin).toInt // TODO: long version necessary? probably not, since differences to window borders should never get that big.
      val lenR = (w.indexMax - anchor.pos).toInt 
      val (nx, ny, kNeighborDist, seenL, seenR) = anchor.neighbors.getNeighborhood(lenL, lenR)
      //println(f"id = ${anchor.pos}%3d    x = ${anchor.x}%8.2f y = ${anchor.y}%8.2f    $nx%4d $ny%4d    neighbor dist:    $kNeighborDist%8.2f    $lenL%4d    $lenR%4d")
      digamma(nx+1) + digamma(ny+1)
    }
    
    val expValue = expValues.sum / expValues.length
    val mi = ( digamma(k) - expValue + digamma(w.size) ) / math.log(2)
    EstimationResult(mi, w.size)
  }
  
  def addData(x: Double, y: Double) {

    // first show the data point to all existing query anchors
    queryAnchors.foreach(anchor => anchor.neighbors.addNeighbor(x, y, true))

    // create new anchor
    val newAnchor = new QueryAnchor(queryAnchors.length, x, y, new NeighborSummary2DIncremental_v2(x, y, k)) // new NeighborSummary2DReference(x, y, k)
    
    // reverse initialization
    queryAnchors.reverse.foreach(anchor => newAnchor.neighbors.addNeighbor(anchor.x, anchor.y, false))
    // caution: 
    // super important to reverse iterate over existing query anchors!!!
    // otherwise the closest-to-left is the oldest, which is actually farther-to-left...

    // append new anchor
    queryAnchors += newAnchor
  }
  
  def length = queryAnchors.length

  val referenceImplementation = true
  val minPossibleQuerySize = k+1
  
}

class EstimatorStreamReferenceIncrementalFactory(k: Int) extends EstimatorStreamFactory {
  def build() = new EstimatorStreamReferenceIncremental(k, this)
  val estimatorDescription = "ReferenceEstimator"
}



/**
 * 3. BASELINE: (no reference in the sense of exact estimation!)
 * A trivial "stream" estimator that performs a static estimation on a certain reservoir 
 */
class EstimatorStreamReferenceReservoir(val factory: EstimatorStreamReferenceReservoirFactory, estimator: EstimatorStatic, reservoirFactory: () => Reservoir[(Long, Double, Double)]) extends EstimatorStream {

  var pos = 0
  val reservoir = reservoirFactory()
  
  def miQueryImpl(w: WindowSpec): EstimationResult = {
    val dataInWindow = reservoir.iterator.toArray.filter(e => w.indexFilter(e._1)).map(e => (e._2, e._3))
    if (dataInWindow.length == 0) {
      return EstimationResult(Double.NaN, 0)
    } else {
      val Array(dataX, dataY) = dataInWindow.map(t => Array(t._1, t._2)).transpose
      val mi = estimator.calcMI(dataX, dataY)
      return EstimationResult(mi, dataInWindow.size)
    }
  }
  
  def addData(x: Double, y: Double) {
    reservoir.add((pos,x,y))
    pos += 1
  }
  
  def length = pos

  val referenceImplementation = false
  val minPossibleQuerySize = estimator.minPossibleQuerySize
  
}

class EstimatorStreamReferenceReservoirFactory(estimator: EstimatorStatic, sampling: String, param: Int) extends EstimatorStreamFactory {
  def build() = {
    val factory = sampling match {
      case "RS" => () => new TraditionalReservoirSampling[(Long, Double, Double)](param)
      case "SW" => () => new SlidingWindowSampling[(Long, Double, Double)](param)
      case "MS_F" => () => new IncrementalReciprocalReservoirSamplingFixedSample[(Long, Double, Double)](param)
      case "MS_D" => () => new IncrementalReciprocalReservoirSamplingOptimized[(Long, Double, Double)](param)
    }
    new EstimatorStreamReferenceReservoir(this, estimator, factory)
  }
  val estimatorDescription = f"StaticRes_${sampling}_$param"
}











