package dmf.stream.mutinf

import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import dmf.SortedBufferKeepHigh
import dmf.Helpers
import dmf.Implicits._
import dmf.ExtendedRandom
import dmf.Stopwatch


trait Reservoir[T] {
  private var seen = 0L
  def add(x: T) {
    val seenBefore = seen
    seen += 1
    addData(seenBefore, x, seenBefore, seen)  // the timestamp "happens to be" identical to seenBefore :)
  }
  def numSeen = seen
  def convertTimestampsToRelative(ts: Long) = -(ts-seen+1)
  def convertTimestampsToRelative(tuple: (Long, T)): (Long, T) = (convertTimestampsToRelative(tuple._1), tuple._2)
  
  /**
   * The redundancy of "seenBefore" and "seenAfter" (which obviously always just differ by one) is on purpose just to make the semantic of "seen" during an "add" 100% clear.
   * There is no official semantic of the time stamp "ts", i.e., a client should always use convertTimestampsToRelative (internally counting starts with 0, so it is just equal to "seenBefore")
   */
  protected def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long)
  def getDistributionTheo(t: Int, s: Int): Double
  def getExpectedSize(streamLength: Long): Double

  /** 1. possibility to access the elements: returns all elements in an array and calculates the position which follows a "relative-reverse-from-current-time" convention, i.e. 0=now, 1=1-step-in-the-past, ... */
  def getReservoir(): Array[(Long, T)]
  /** 2. possibility to access the elements: simple iterator, in general iteration order is old-to-new, and no relative-position-calculation */
  def iterator(): Iterator[T]
}


/** 
 * (uniform) deletion sampling gives a EXPONENTIAL distribution over time, with exp(-1/reservoirSize * t)
 */
class DeleteReservoirSampling[T](size: Int) extends Reservoir[T] {
  val buffer = ArrayBuffer[(Long, T)]()
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer += ((ts, x))
    if (buffer.length > size) {
      val randInd = Random.nextInt(buffer.length)
      buffer.remove(randInd)
    }
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.iterator.map(_._2)
  
  val pDelete = 1.0/(size+1)            // must be divided by size+1 because the selection is always among B+1 elements, since the buffer was extended temporarily 
  def getDistributionTheo(t: Int, s: Int) = 
    //math.exp(-1.0/size*t)               // first version
    //math.exp(-pDelete*(t+1))            // since the true distribution is not quite a geometric distribution a conversion to an exponential distribution is not possible?!
                                          // correction: it is possible: for small pDelete, (1-p) can be compared to the taylor series of e^-p which gives the exp function as upper bound.
    math.pow(1.0 - pDelete, t+1)          // must be t+1 because even the point at t=0 is included _once_ in the deletion process
                                          // this only works in the limit of long streams because otherwise the end of the buffer contains the probability mass of the tail...
    //math.exp(math.log(1-pDelete)*(t+1)) // obviously we can still express it as exponential e^(ln(1-p)*t) with the proper exponent; again since ln(1-p) ~ -p the e^(-pt) version works approximately
  def getExpectedSize(streamLength: Long): Double = size
}

/** 
 * Sliding window sampling gives a BOX distribution over time
 */
class SlidingWindowSampling[T](size: Int) extends Reservoir[T] {
  val buffer = ListBuffer[(Long, T)]()
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer += ((ts, x))
    if (buffer.length > size) {
      buffer.remove(0)
    }
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.iterator.map(_._2)
  def getDistributionTheo(t: Int, s: Int) = if (t < size) 1.0 else 0.0
  def getExpectedSize(streamLength: Long): Double = size
}

/** 
 * Traditional reservoir sampling gives a UNIFORM distribution over time
 */
class TraditionalReservoirSampling[T](size: Int) extends Reservoir[T] {
  val buffer = ArrayBuffer[(Long, T)]()
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    if (buffer.length < size) {
      buffer += ((ts, x))
    } else {
      val randInd = Random.nextInt(seenAfter.toInt) // reservoir sampling requires index [0, ..., seenBefore], since nextInt is exclusive seenAfter as upper limit 
      if (randInd < size) {
        buffer(randInd) = ((ts, x))
      }
    }
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.sortBy(_._1).iterator.map(_._2) // sorting is necessary since reservoir sampling does not maintain proper order
  def getDistributionTheo(t: Int, s: Int) = (size.toDouble/s)
  def getExpectedSize(streamLength: Long): Double = size
}


/** 
 * implementation according to the paper on weighted sampling...
 * small gamma (0.0) gives uniform sampling (actually exp with zero slope)
 * large gamma (0.1) gives sliding window sampling
 */
class ArbitraryReservoirSampling[T](size: Int, gamma: Double) extends Reservoir[T] {
  val buffer = new SortedBufferKeepHigh[(Double, Long, T)](size)(new Ordering[(Double, Long, T)] {
    def compare(a: (Double, Long, T), b: (Double, Long, T)) = math.Ordering.Double.compare(a._1, b._1)
  })
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    val u = Random.nextDouble
    val w = math.exp(gamma*seenAfter) // TODO: check seen semantic
    val k = math.pow(u, 1.0/w)
    //println(x, u, w, k)
    buffer.add((k, ts, x))
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.sortedList.map(t => (convertTimestampsToRelative(t._2), t._3)).toArray
  }
  def iterator = buffer.iterator.map(_._3)
  def getDistributionTheo(t: Int, s: Int) = (size.toDouble/s) // TODO
  def getExpectedSize(streamLength: Long): Double = size
}
/** 
 * Quadratic version: 
 */
class QuadraticReservoirSampling[T](size: Int) extends Reservoir[T] {
  val buffer = new SortedBufferKeepHigh[(Double, Long, T)](size)(new Ordering[(Double, Long, T)] {
    def compare(a: (Double, Long, T), b: (Double, Long, T)) = math.Ordering.Double.compare(a._1, b._1)
  })
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    val u = Random.nextDouble
    val w = seenAfter*seenAfter // TODO: check seen semantic
    val k = math.pow(u, 1.0/w)
    //println(x, u, w, k)
    buffer.add((k, ts, x))
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.sortedList.map(t => (convertTimestampsToRelative(t._2), t._3)).toArray
  }
  def iterator = buffer.iterator.map(_._3)
  def getDistributionTheo(t: Int, s: Int) = (size.toDouble/s) // TODO
  def getExpectedSize(streamLength: Long): Double = size
}


/**
 * Weighting the index-to-remove does not give great results:
 * - using larger weights for older samples works, and goes from the pure exp-dist towards the sliding-window-dist (more weight on recent data)
 * - BUT: the other way around allows to leave a certain number of really old objects in the reservoir (those from the very beginning), which does not make sense (no uniform dist)
 */
class ReservoirSamplingByWeightedDelete[T](size: Int) extends Reservoir[T] {
  val buffer = ArrayBuffer[(Long, T)]()
  
  def weightedRandInd(numPossibilities: Int, wL: Double, wR: Double) = {
    val u = Random.nextDouble
    val weights = Range(0, numPossibilities).map(i => wL + (wR-wL)*i/(numPossibilities-1)).toArray
    val weightsSum = weights.sum
    val randInd = ExtendedRandom.nextIntWeighted(weights.map(_ / weightsSum))
    //println(weights, randInd)
    randInd
  }
  
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer += ((ts, x))
    if (buffer.length > size) {
      val randInd = weightedRandInd(buffer.length, 1.0, 0.0)
      buffer.remove(randInd)
    }
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.iterator.map(_._2)
  def getDistributionTheo(t: Int, s: Int) = math.exp(-1.0/size*t) // TODO
  def getExpectedSize(streamLength: Long): Double = size
}

/**
 * Mixing between EXPONENTIAL, BOX, and UNIFORM
 * Works, but is this what I want? 
 */
class MixtureOfSamplingMethods[T](size: Int, w1: Double, w2: Double) extends Reservoir[T] {
  val buffer = ArrayBuffer[(Long, T)]()
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    val choice = Random.nextDouble
    if (choice < w1) {
      // EXPONENTIAL 
      buffer += ((ts, x))
      if (buffer.length > size) {
        val randInd = Random.nextInt(buffer.length)
        buffer.remove(randInd)
      }
    } else if (choice < w1+w2) { 
      // BOX
      buffer += ((ts, x))
      if (buffer.length > size) {
        buffer.remove(0)
      }
    } else {
      // UNIFORM
      if (buffer.length < size) {
        buffer += ((ts, x))
      } else {
        val randInd = Random.nextInt(seenAfter.toInt)
        if (randInd < size) {
          buffer(randInd) = ((ts, x))
        }
      }      
    }
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.iterator.map(_._2)
  def getDistributionTheo(t: Int, s: Int) = math.exp(-1.0/size*t) // TODO
  def getExpectedSize(streamLength: Long): Double = size
}


// -----------------------------------------------------------------------------------------------------------------------
// Reciprocal
// -----------------------------------------------------------------------------------------------------------------------

/**
 * This reciprocal version is exact but only calculated on query
 */
class ExactReciprocalReservoirSampling[T](size: Int) extends Reservoir[T] {
  val buffer = ArrayBuffer[(Long, T)]()
  
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer += ((ts, x))
  }
  /** This version is correct, but does not return a fixed-length reservoir */
  def getReservoir(): Array[(Long, T)] = {
    val weights = Range(0, buffer.length).map(t => 1.0 / (t+1)).reverse.toArray
    val convertedBuffer = buffer.map(t => convertTimestampsToRelative(t))
    val sample = Range(0, buffer.length).filter(i => Random.nextDouble < weights(i)).map(i => convertedBuffer(i))
    sample.toArray
  }
  def iterator = buffer.iterator.map(_._2)
  /** This version is wrong: 
   *  It is not correct to generate weights according to 1/x and scale them to 1 to get probabilities; 
   *  each 1/x term itself must be treated as a probability  */
  def getReservoirBugged(): Array[(Long, T)] = {
    val weights = Range(0, buffer.length).map(t => 1.0 / (t+1)).toArray
    val weightsSum = weights.sum
    val weightsNormed = weights.map(_ / weightsSum)
    val alreadyDrawn = collection.mutable.Set[Int]()
    val sample = Range(0, size).map { _ =>
      // ugly way to ensure "without replacement"
      var randInd = ExtendedRandom.nextIntWeighted(weightsNormed)
      while (alreadyDrawn.contains(randInd)) {
        randInd = ExtendedRandom.nextIntWeighted(weightsNormed)
      }
      alreadyDrawn += randInd
      val ind = buffer.length - 1 - randInd
      val t = buffer(ind)._1
      val x = buffer(ind)._2
      (convertTimestampsToRelative(t), x)
    }
    sample.toArray
  }
  def getDistributionTheo(t: Int, s: Int) = 
    //math.exp(-1.0/size*t)
    1.0 / (t+1)
  def getExpectedSize(streamLength: Long): Double = -999 // TODO, size is not fixed...  
}

/**
 * Sophisticated reciprocal sampling, by incremental processing scheme
 */
class IncrementalReciprocalReservoirSampling[T](alpha: Double) extends Reservoir[T] {
  var buffer = ArrayBuffer[(Long, T)]()
  val debug = false
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer += ((ts, x))
    if (debug) println(f"adding element with timestamp $ts%10d    [seen = $seenAfter%10d]:")
    val valuesToRemove = collection.mutable.Set[Long]()
    buffer.foreach{ b =>
      val t = convertTimestampsToRelative(b._1) // seen-b._1-1
      val n = t+1       // n's start at 1, which avoids the singularity at 0
      def p(xn: Double) = if (xn < 1) 1.0 else 1.0/xn
      val xn = n/alpha
      val pn = p(xn) 
      val xnMinus1 = (n-1)/alpha
      val pnMinus1 = p(xnMinus1) 
      val probToStay = if (xn < 1) 1.0 else pn / pnMinus1
      val stay = Random.nextDouble < probToStay
      if (!stay) 
        valuesToRemove += b._1
      if (debug) println(f"object with timestamp = ${b._1}%10d    rel_t = $t%10d    n = $n%10d    xn = $xn%12.3f    p_stay = $probToStay%12.3f    stay = ${if (stay) "1" else "0"}")
    }
    buffer = buffer.filter(b => !valuesToRemove.contains(b._1))
    if (debug) println("buffer after insert: " + buffer)
  }
  def getReservoir(): Array[(Long, T)] = {
    buffer.map(t => convertTimestampsToRelative(t)).toArray
  }
  def iterator = buffer.iterator.map(_._2)
  def getDistributionTheo(t: Int, s: Int) = { 
    //math.exp(-1.0/size*t)
    //1.0 / (t+1)
    //1.0 / (t)       // not correct because the theoretical distribution must be defined at t=0
    val n = t+1       // n's start at 1
    val xn = n/alpha
    if (xn < 1) 1.0 else 1.0/xn
  }
  def getExpectedSize(streamLength: Long): Double = {
    val N = streamLength.toInt
    val k0 = math.floor(alpha).toInt
    val ES = k0 + alpha * Range(k0+1, N+1).map(k => 1.0/k).sum
    ES
  }  
}

/**
 * Same as before, but optimized implementation
 */
class IncrementalReciprocalReservoirSamplingOptimized[T](alpha: Double) extends Reservoir[T] {
  // all timing information is w.r.t. a stream length of 10'000 and alpha=0.01
  var buffer = new java.util.LinkedList[(Long, T)]()    // alternatives? java.util.LinkedList (14.6 us), java.util.HashSet (20.3 us), java.util.ArrayList (14.4 us)
  val debug = false
  // persisting the probability did not help: performance decrease from 14.6 us to 80.0 us
  object PresistentPn {
    private val calculated = collection.mutable.Map[Long, Double]()
    def p(n: Long): Double = {
      val xn = n/alpha
      if (xn < 1) 1.0 else 1.0/xn
    }
    def get(n: Long): Double = {
      calculated.getOrElseUpdate(n, p(n))
    }
  }
  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    buffer.add((ts, x))
    if (debug) println(f"adding element with timestamp $ts%10d    [seen = $seenAfter%10d]:")
    val iter = buffer.iterator()
    while (iter.hasNext()) {
      val b = iter.next()
      val t = convertTimestampsToRelative(b._1) // seen-b._1-1
      val n = t+1       // n's start at 1, which avoids the singularity at 0
      def p(xn: Double) = if (xn < 1) 1.0 else 1.0/xn
      val xn = n/alpha
      val pn = p(xn)
      // val pn = PresistentPn.get(n)
      // short circuit possible: if probability is >= 1, no necessity to remove (but only slight improvement from 14.7 us to 14.6 us)
      if (pn < 1.0) {
        val xnMinus1 = (n-1)/alpha
        val pnMinus1 = p(xnMinus1) 
        //val pnMinus1 = PresistentPn.get(n-1)
        val probToStay = if (xn < 1) 1.0 else pn / pnMinus1
        val stay = Random.nextDouble < probToStay
        if (!stay) {
          iter.remove()
        }
        if (debug) println(f"object with timestamp = ${b._1}%10d    rel_t = $t%10d    n = $n%10d    xn = $xn%12.3f    p_stay = $probToStay%12.3f    stay = ${if (stay) "1" else "0"}")
      }
    }
    if (debug) println("buffer after insert: " + buffer)
  }
  def getReservoir(): Array[(Long, T)] = {
    import scala.collection.JavaConversions._
    //val arr = Array[(Long, T)](buffer.iterator() :_*)
    //val arr = buffer.toArray.map(_.asInstanceOf[(Long, T)])
    //buffer.toArray.asInstanceOf[Array[(Long, T)]].map(t => convertTimestampsToRelative(t))
    buffer.toArray.map(_.asInstanceOf[(Long, T)]).map(t => convertTimestampsToRelative(t))
  } 
  def iterator = {
    import scala.collection.JavaConversions._
    buffer.iterator.map(_._2)
  }
  def getDistributionTheo(t: Int, s: Int) = { 
    //math.exp(-1.0/size*t)
    //1.0 / (t+1)
    //1.0 / (t)       // not correct because the theoretical distribution must be defined at t=0
    val n = t+1       // n's start at 1
    val xn = n/alpha
    if (xn < 1) 1.0 else 1.0/xn
  }
  def getExpectedSize(streamLength: Long): Double = {
    val N = streamLength.toInt
    val k0 = math.floor(alpha).toInt
    val ES = k0 + alpha * Range(k0+1, N+1).map(k => 1.0/k).sum
    ES
  }  
}


/**
 * Incremental reciprocal sampling, but with fixed sample size (based on optimized version)
 */
class IncrementalReciprocalReservoirSamplingFixedSample[T](size: Int) extends Reservoir[T] {
  
  var alpha = size.toDouble
  var buffer = new java.util.LinkedList[(Long, T)]()
  val debug = false
  
  def p(xn: Double) = if (xn < 1) 1.0 else 1.0/xn

  def addData(ts: Long, x: T, seenBefore: Long, seenAfter: Long) {
    if (seenAfter <= size) {
      buffer.add((ts, x))
    } else {
      buffer.add((ts, x))
      if (debug) println(f"adding element with timestamp $ts%10d    [seen = $seenAfter%10d]:")
      val iter = buffer.iterator()
      while (iter.hasNext()) {
        val b = iter.next()
        val t = convertTimestampsToRelative(b._1)
        val n = t+1       // n's start at 1, which avoids the singularity at 0
        val xn = n/alpha
        val pn = p(xn)
        // short circuit possible: if probability is >= 1, no necessity to remove (but only slight improvement from 14.7 us to 14.6 us)
        if (pn < 1.0) {
          val xnMinus1 = (n-1)/alpha
          val pnMinus1 = p(xnMinus1) 
          val probToStay = if (xn < 1) 1.0 else pn / pnMinus1   // this check still needed?
          val stay = Random.nextDouble < probToStay
          if (!stay) {
            iter.remove()
          }
          if (debug) println(f"object with timestamp = ${b._1}%10d    rel_t = $t%10d    n = $n%10d    xn = $xn%12.3f    p_stay = $probToStay%12.3f    stay = ${if (stay) "1" else "0"}")
        }
      }
    }
    if (seenAfter > size && seenAfter % 100 == 0) {
      adjustAlpha(seenAfter)
    }
    if (debug) println("buffer after insert: " + buffer)
  }
  def adjustAlpha(T: Long) {
    val oldAlpha = alpha
    val newAlpha = IterativeSolverOfAlphaSampleSizeEquation.solveForAlphaGivenS(size, T, approx=true)._1
    val oldSizeOfBuffer = buffer.size()
    val iter = buffer.iterator()
    while (iter.hasNext()) {
      val b = iter.next()
      val t = convertTimestampsToRelative(b._1)
      val n = t+1       // n's start at 1, which avoids the singularity at 0
      val oldP = p(n/oldAlpha)
      val newP = p(n/newAlpha)
      /*
      val pDiff = oldP-newP
      // println(f"n = $n%6d    oldP = $oldP%12.6f    newP = $newP%12.6f    diff = $pDiff%12.6f")
      if (pDiff > 0.0) {
        val delete = Random.nextDouble < pDiff
        if (delete) {
          iter.remove()
        }
      }
      */
      val pRatio = newP / oldP
      // println(f"n = $n%6d    oldP = $oldP%12.6f    newP = $newP%12.6f    diff = $pRatio%12.6f")
      if (pRatio < 1.0 && pRatio > 0.0) {
        val stay = Random.nextDouble < pRatio
        if (!stay) {
          iter.remove()
        }
      }
    }
    val newSizeOfBuffer = buffer.size()
    // println(f"T = $T%12d    oldAlpha = $oldAlpha%12.3f    newAlpha = $newAlpha%12.3f    oldSizeOfBuffer = $oldSizeOfBuffer%6d    newSizeOfBuffer = $newSizeOfBuffer%6d")
    alpha = newAlpha
  }
  
  def getReservoir(): Array[(Long, T)] = {
    import scala.collection.JavaConversions._
    buffer.toArray.map(_.asInstanceOf[(Long, T)]).map(t => convertTimestampsToRelative(t))
  } 
  def iterator = {
    import scala.collection.JavaConversions._
    buffer.iterator.map(_._2)
  }
  def getDistributionTheo(t: Int, T: Int) = {
    val properAlpha = IterativeSolverOfAlphaSampleSizeEquation.solveForAlphaGivenS(size, T, approx=true)._1
    val n = t+1       // n's start at 1
    val xn = n/properAlpha
    if (xn < 1) 1.0 else 1.0/xn
  }
  def getExpectedSize(streamLength: Long): Double = {
    size
  }  
}




object IterativeSolverOfAlphaSampleSizeEquation{


  // exact version of HnCapped
  def HnCapped(alpha: Double, T: Long) = (1L to T).map(k => alpha/k).map(x => if (x > 1) 1.0 else x).sum             // a Long Range(1, T+1), i.e. with last element equal to T, is either (1 until T+1) or (1 to T), i.e. "to" is inclusive

  // approximate version of HnCapped (using approximations of harmonic numbers)
  def HnCappedApprox(alpha: Double, T: Long) = alpha.floor + alpha*(HnApprox(T) - HnApprox(alpha.floor))

  // approximation of harmonic numbers:
  val EulerMascheroni = 0.5772156649015328606065120
  private def HnApprox(n: Double) = math.log(n) + EulerMascheroni + 1.0/(2.0*n) - 1.0/(12.0*n*n) + 1.0/(120.0*n*n*n*n) 

  
  def solveForAlphaGivenS(S: Double, T: Long, approx: Boolean = true): (Double, Boolean, Int) = {
    
    val HnFunction = if (approx) HnCappedApprox _ else HnCapped _
    
    if (T <= S) {
      return (S, true, 0)
    }
    var alpha = 1.0
    //var found = false
    var iters = 1
    val maxIters = 1000
    var lastHnCapped = 0.0 
    while (math.abs(S - lastHnCapped) > 0.0001 && iters < maxIters) {
      
      lastHnCapped = HnFunction(alpha, T)
      
      // val alphaNext = alpha * S / lastHnCapped
      val alphaNext = alpha * (S-alpha.floor) / (lastHnCapped-alpha.floor)
      
      //println(f"a_$iters%-5d = $alpha%12.6f    HnCapped = ${HnCapped(alpha)}%8.1f    numCapped = ${math.floor(alpha).toInt}%8d    =>    a_${iters+1}%-5d = $alphaNext%12.6f    HnCapped = ${HnCapped(alphaNext)}%8.1f    numCapped = ${math.floor(alphaNext).toInt}%8d      diff = ${S - HnCapped(alphaNext)}")
      alpha = alphaNext
      iters += 1
    }
    val converge = !alpha.isInfinity && !alpha.isInfinite() && !alpha.isNaN() && iters != maxIters
    return (alpha, converge, iters)
  }
  
  def solveForSGivenAlpha(alpha: Double, T: Long, approx: Boolean = true): Double = {
    if (T < alpha) {
      return alpha // convention is to simply return the given alpha
    } else {
      if (approx) {
        return HnCappedApprox(alpha, T)
      } else {
        return HnCapped(alpha, T)
      }
    }
  }
  
  def main(args: Array[String]) {
    
    // some tests with numeric output
    if (false) {
      val Ts = List(10, 100, 101, 200, 500, 1000, 2000, 10000, 100000, 1000000, 10000000)
      val Ss = List(100, 500, 1000)
      for (S <- Ss) {
        for (T <- Ts) {
          val (alpha, converged, iterations) = solveForAlphaGivenS(S, T, approx=true)
          if (converged) {
            val trueS = solveForSGivenAlpha(alpha, T, approx=false)
            println(f"S = $S%6d    T = $T%12d    alpha = $alpha%12.6f    error: ${S - trueS}%18.12f    iterations: $iterations%6d")
          }
        }
        println()
      }
    }

    // generate plots
    if (true) {
      val numSteps = 200
      val curves = Array(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000).map { S =>       // 100, 1000, 10000
        val upperLimit = 1000L * 1000 * 1000 * 1000
        val Ts = Helpers.Logspace(S, upperLimit, numSteps).map(_.toLong).toArray
        val alphas = Ts.map { T =>
          val (alpha, converged, iterations) = solveForAlphaGivenS(S, T, approx=true)
          (T.toDouble, alpha)
        }
        (f"$$S = $S$$", alphas)
      }
      // MatPlotLib.plotLines(curves, Some(Map("logXAxis" -> true, "linesOnly" -> true)))
      // MatPlotLib.plotLines(curves, Some(Map("logXAxis" -> true, "linesOnly" -> true, "interactive" -> false, "outFileNames" -> List("test.pdf", "test.svg"))))
    }
  }
  
}


















