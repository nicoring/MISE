package dmf.stream.mutinf

import scala.util.Random
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.linear.MatrixUtils
import dmf.Helpers
import dmf.Implicits._


trait StaticDataGenerator {
  def generate(N: Int): (Array[Double], Array[Double], Double)
}



/**
 * Data Generator producing a single (two-dimensional) gaussian distribution
 * MI can be calculated analytically in this case
 */
class GeneratorSingleGaussian(val covariance: Double) extends StaticDataGenerator {

  override def toString() = f"SingleGaussian_${covariance}_${theoreticalMI}%.4f"

  val theoreticalMI = -0.5 * math.log(1 - covariance*covariance) / math.log(2) // Eq. 11 from Kraskov

  def generate(N: Int): (Array[Double], Array[Double], Double) = {
    val generator = new MultivariateNormalDistribution(Array(0d, 0d), Array(Array(1d, covariance), Array(covariance, 1d)))
    val data = Array.tabulate(N)(i => generator.sample())
    return (data.map(_(0)), data.map(_(1)), theoreticalMI)
  }
}

object GeneratorSingleGaussian {
  def apply() = new GeneratorSingleGaussian(Helpers.readUserInput("covariance = ", 0.95))
}


/**
 * Data Generator producing a mixture of Gaussians
 * MI calculation: TODO
 */
class GeneratorGaussianMixture(val numComponents: Int) extends StaticDataGenerator {

  override def toString() = f"GaussianMixture_${numComponents}"

  def generate(N: Int): (Array[Double], Array[Double], Double) = {
    val generators = Array.tabulate(numComponents){_ => 
      val corr = Random.nextDouble
      val corrMat = MatrixUtils.createRealMatrix(Array(Array(1.0, corr), Array(corr, 1.0)))
      val diagMat = MatrixUtils.createRealMatrix(Array(Array(Random.nextDouble, 0.0), Array(0.0, Random.nextDouble)))
      val covMat = diagMat multiply corrMat multiply diagMat
      new MultivariateNormalDistribution(Array(Random.nextDouble, Random.nextDouble), covMat.getData())
    }
    val data = Array.tabulate(N){i =>
      val randGen = Random.nextInt(generators.length)
      generators(randGen).sample()
    }
    return (data.map(_(0)), data.map(_(1)), 0.0)
  }
}

object GeneratorGaussianMixture {
  def apply() = new GeneratorGaussianMixture(Helpers.readUserInput("numComponents = ", 3))
}



/**
 * Data Generator producing a mixture of uniform distributions
 * MI calculation: TODO
 */
class GeneratorUniformMixture(val numComponents: Int) extends StaticDataGenerator {

  override def toString() = f"UniformMixture_${numComponents}"
    
  implicit class WithAlmostEquals(x: Double) {
    def ~=(that: Double): Boolean = math.abs(x - that) < 0.00001
  }
    
  class Box(val x1: Double, val x2: Double, val y1: Double, val y2: Double) {
    assert(x1 < x2 && y1 < y2)
    val wx = (x2-x1)
    val wy = (y2-y1)
    val volume = wx*wy
    def inBox(x: Double, y: Double): Boolean = (x1 <= x && x <= x2 && y1 <= y && y <= y2)
    def getDensityAt(x: Double, y: Double, cellVolume: Double): Double = {
      if (inBox(x,y)) {
        cellVolume / volume
      } else {
        0.0
      }
    }
    def sample(): (Double, Double) = (x1 + wx*Random.nextDouble, y1 + wy*Random.nextDouble) 
  }
  object Box {
    def apply(x1: Double, x2: Double, y1: Double, y2: Double): Box = {
      val xMin = x1 min x2
      val xMax = x1 max x2
      val yMin = y1 min y2
      val yMax = y1 max y2
      new Box(xMin, xMax, yMin, yMax)
    }
    def randomBox() = Box(Random.nextDouble, Random.nextDouble, Random.nextDouble, Random.nextDouble)
  }
  
  def calcTheoreticalMI(boxes: Array[Box]): Double = {
    val gridLinesX = boxes.foldLeft(Set[Double]()){ case (set, box) => set + box.x1 + box.x2 }.toArray.sorted
    val gridLinesY = boxes.foldLeft(Set[Double]()){ case (set, box) => set + box.y1 + box.y2 }.toArray.sorted
    
    val densXY = Array.tabulate(gridLinesX.length - 1, gridLinesY.length - 1){ (i,j) =>
      val x = 0.5 * (gridLinesX(i) + gridLinesX(i+1))
      val y = 0.5 * (gridLinesY(j) + gridLinesY(j+1))
      val cellVolume =  (gridLinesX(i+1)-gridLinesX(i)) * (gridLinesY(j+1)-gridLinesY(j))
      val densities = boxes.map(b => 1.0 / numComponents * b.getDensityAt(x, y, cellVolume)) // TODO: extend to arbitrary weights
      densities.reduce(_ + _)
    }

    val densX = densXY.map(row => row.reduce(_ + _))
    val densY = densXY.transpose.map(row => row.reduce(_ + _))
    
    val densXYSum = densXY.flatten.reduce(_ + _)
    assert(densXYSum ~= 1.0)
    assert(densX.sum ~= 1.0)
    assert(densY.sum ~= 1.0)
    
    var mi = 0.0
    val log2 = math.log(2)
    for (i <- Range(0, gridLinesX.length - 1)) {
      for (j <- Range(0, gridLinesY.length - 1)) {
        val pXY = densXY(i)(j)
        val pX = densX(i)
        val pY = densY(j)
        //println(pXY, pX, pY, pXY * math.log(pXY / (pX*pY))/log2)
        if (pXY != 0) {
          mi += pXY * math.log(pXY / (pX*pY))/log2
        }
      }
    }
    mi
  }
  
  def generate(N: Int): (Array[Double], Array[Double], Double) = {
    val boxes = Array.tabulate(numComponents)(_ => Box.randomBox())
    val data = Array.tabulate(N){i =>
      val randGen = Random.nextInt(boxes.length)
      boxes(randGen).sample()
    }
    val theoreticalMI = calcTheoreticalMI(boxes)
    return (data.map(_._1), data.map(_._2), theoreticalMI)
  }
}

object GeneratorUniformMixture {
  def apply() = new GeneratorUniformMixture(Helpers.readUserInput("numComponents = ", 3))
}










