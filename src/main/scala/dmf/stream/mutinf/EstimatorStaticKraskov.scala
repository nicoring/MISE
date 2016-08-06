package dmf.stream.mutinf

import org.apache.commons.math3.special.Gamma.digamma
import dmf.Helpers


class Neighbor2D(val ind: Int, val distanceX: Double, val distanceY: Double) extends Ordered[Neighbor2D] {
  val distance: Double = {
    distanceX max distanceY
  }
  def compare(that: Neighbor2D) = Ordering.Double.compare(this.distance, that.distance)
  override def toString(): String = {
    "N(%d, %f, %f, %f)".format(ind, distanceX, distanceY, distance)
  }
}


class Neighbor2DFactory(dataX: Array[Double], dataY: Array[Double]) {
  
  val N = dataX.length

  /**
   * Returns the k-neighborhood of a given object id
   */
  def getNeighborhoodOf(i: Int, k: Int): IndexedSeq[Neighbor2D] = {
    
    val neighbors = Range(0, N).filter(j => j != i).map{j =>
      val distX = math.abs(dataX(i) - dataX(j)) 
      val distY = math.abs(dataY(i) - dataY(j)) 
      new Neighbor2D(j, distX, distY)
    }.sorted.take(k)
    
    return neighbors
  }

}



object EstimatorStaticKraskov {
  def apply(): EstimatorStaticKraskov = {
    val k = Helpers.readUserInput("k = ", 1)
    val speedup = Helpers.readUserInput[Option[Double]]("speedup = ", None)
    new EstimatorStaticKraskov(k, speedup)
  }
}

class EstimatorStaticKraskov(val k: Int, speedup: Option[Double] = None) extends EstimatorStatic {
  
  val minPossibleQuerySize = k+1
  
  //val firstVariant = Helpers.readUserInput("firstVariant = ", true)
  override def toString() = f"KraskovEstimator_${k}_${speedup}"
  
  def calcMI(dataX: Array[Double], dataY: Array[Double]): Double = {
    
    val (sortedIndicesX, sortedRanksX) = Helpers.sortIndicesAndRanks(dataX)
    val (sortedIndicesY, sortedRanksY) = Helpers.sortIndicesAndRanks(dataY)
    
    val N = dataX.length
    if (N <= k) {
      return Double.NaN
    }
    
    val knn = new Neighbor2DFactory(dataX, dataY)
    
    def countInOneDim(i: Int, data: Array[Double], ranks: Array[Int], indices: Array[Int], epsilonHalf: Double): Int = {
      val value = data(i)
      val rank = ranks(i)
      var count = 0 // count the point itself? -> no
      
      // count upwards
      var next = rank + 1
      while (next < N && math.abs(value - data(indices(next))) < epsilonHalf) {
        count += 1
        next += 1
      }
      
      // count downwards
      next = rank - 1
      while (next >= 0 && math.abs(value - data(indices(next))) < epsilonHalf) {
        count += 1
        next -= 1
      }
      
      count
    }
    
    val numToTake = speedup match {
      case Some(number) => (N*number).toInt
      case _            =>  N
    }
    
    val expValues = Range(0, numToTake).toArray.map { i =>
      val neighbor = knn.getNeighborhoodOf(i, k).last
      val epsHalf = neighbor.distance
      val nx = countInOneDim(i, dataX, sortedRanksX, sortedIndicesX, epsHalf)
      val ny = countInOneDim(i, dataY, sortedRanksY, sortedIndicesY, epsHalf)
      //println("id = %3d    x = %8.2f y = %8.2f    %4d %4d    neighbor:    %3d    %8.2f    ".format(i, dataX(i), dataY(i), nx, ny, neighbor.ind, epsHalf))
      //println("distances to other points: " + dataX.toList.indices.map(j => math.abs(dataX(i)-dataX(j) max math.abs(dataY(i)-dataY(j)))))
      if (false) {
        // to test whether those nx/ny values are okay...
        val trueNx = dataX.indices.filter(_ != i).map(dataX(_)).filter(x => math.abs(x-dataX(i)) < epsHalf).length
        val trueNy = dataY.indices.filter(_ != i).map(dataY(_)).filter(y => math.abs(y-dataY(i)) < epsHalf).length
        if (trueNx != nx) println(f"fuck nx should be $trueNx instead of $nx")
        if (trueNy != ny) println(f"fuck ny should be $trueNy instead of $ny")
        assert(trueNx == nx)
        assert(trueNy == ny)
      }
      digamma(nx+1) + digamma(ny+1)
    }
    
    val expValue = expValues.sum / expValues.length
    
    //MatPlotLib.plotScatter(dataX, dataY, Some(expValues))
    //MatPlotLib.plotDistribution(expValues)
    
    return ( digamma(k) - expValue + digamma(N) ) / math.log(2)
  }  
  
}

