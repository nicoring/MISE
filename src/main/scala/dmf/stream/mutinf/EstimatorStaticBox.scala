package dmf.stream.mutinf

import scala.util.Random
import dmf.Helpers
import dmf.Implicits._


sealed trait PointLocation
case object PointLocationInBox extends PointLocation
case object PointLocationMargX extends PointLocation
case object PointLocationMargY extends PointLocation
case object PointLocationOuter extends PointLocation


class DomainVolume(val x1: Double, val x2: Double, val y1: Double, val y2: Double, val xMin: Double, val xMax: Double, val yMin: Double, yMax: Double) {
  
  val totalX = xMax - xMin
  val totalY = yMax - yMin
  
  val wx = x2 - x1
  val wy = y2 - y1
  
  val volumeInBox = wx*wy
  
  val totalVolumeMargX = wx * (totalY - wy)
  val totalVolumeMargY = wy * (totalX - wx)
  val totalVolumeOuter = (totalX*totalY) - volumeInBox - totalVolumeMargX - totalVolumeMargY

  val volumeMargXL = wx * (y1-yMin) 
  val volumeMargXH = wx * (yMax-y2) 

  val volumeMargYL = wy * (x1-xMin) 
  val volumeMargYH = wy * (xMax-x2) 
  
  val volumeMargOLL = (x1-xMin) * (y1-yMin)
  val volumeMargOHL = (xMax-x2) * (y1-yMin)
  val volumeMargOLH = (x1-xMin) * (yMax-y2)
  val volumeMargOHH = (xMax-x2) * (yMax-y2)
  
  def volumeMargX(y: Double) = {
    /*
    if (y <= y1) {
      volumeMargXL
    } else {
      volumeMargXH
    }
    */
    totalVolumeMargX
  }
  def volumeMargY(x: Double) = {
    /*
    if (x <= x1) {
      volumeMargYL
    } else {
      volumeMargYH
    }
    */
    totalVolumeMargY
  }  
  
  def volumeOuter(x: Double, y: Double) = {
    /*
    if (x <= x1 && y <= y1)
      volumeMargOLL
    else if (x >  x2 && y <= y1)
      volumeMargOHL
    else if (x <= x1 && y >  y2)
      volumeMargOLH
    else
      volumeMargOHH
    */
    totalVolumeOuter
  }
}


class Box(val x1: Double, val x2: Double, val y1: Double, val y2: Double) {

  private var count = 0
  private var cInBox = 0
  private var cMargX = 0
  private var cMargY = 0
  private var cOuter = 0
  
  private var domainVolume: Option[DomainVolume] = None
  
  def wX = x2 - x1
  def wY = y2 - y1
  
  def getCount: Int = count
  
  def getPointLocation(x: Double, y: Double): PointLocation = {
    (x,y) match {
      case (x,y) if (x1 <= x && x <= x2 && y1 <= y && y <= y2) => PointLocationInBox
      case (x,y) if (x1 <= x && x <= x2) => PointLocationMargX
      case (x,y) if (y1 <= y && y <= y2) => PointLocationMargY
      case _ => PointLocationOuter
    }
  }
  
  def getDensityAt(x: Double, y: Double): Double = {
    getPointLocation(x,y) match {
      case PointLocationInBox => cInBox.toDouble / count / domainVolume.get.volumeInBox
      case PointLocationMargX => cMargX.toDouble / count / domainVolume.get.volumeMargX(y)
      case PointLocationMargY => cMargY.toDouble / count / domainVolume.get.volumeMargY(x)
      case PointLocationOuter => cOuter.toDouble / count / domainVolume.get.volumeOuter(x,y)
    }
  }
  
  def addData(x: Double, y: Double) {
    getPointLocation(x,y) match {
      case PointLocationInBox => cInBox += 1
      case PointLocationMargX => cMargX += 1
      case PointLocationMargY => cMargY += 1
      case PointLocationOuter => cOuter += 1
    }
    count += 1
  }
  
  def setDomainVolume(xMin: Double, xMax: Double, yMin: Double, yMax: Double) {
    domainVolume = Some(new DomainVolume(x1, x2, y1, y2, xMin, xMax, yMin, yMax))
  }
  
  override def toString = f"Box($x1%.3f, $x2%.3f, $y1%.3f, $y2%.3f, $cInBox, $cMargX, $cMargY, $cOuter)"
}




class EstimatorStaticBox extends EstimatorStatic {
  
  val minPossibleQuerySize = 1 // TODO
  
  implicit class WithAlmostEquals(x: Double) {
    def ~=(that: Double): Boolean = math.abs(x - that) < 0.001
  }
  
  val numBoxes = Helpers.readUserInput("numBoxes = ", 5)
  
  def calcMI(dataX: Array[Double], dataY: Array[Double]): Double = {
    
    val N = dataX.length

    val absMinX = dataX.min
    val absMaxX = dataX.max
    val absMinY = dataY.min
    val absMaxY = dataY.max

    val widthX = 1.0 * dataX.sdev
    val widthY = 1.0 * dataY.sdev
    val minWidthX = 0.5 * widthX
    val minWidthY = 0.5 * widthY

    val minX = absMinX + widthX
    val maxX = absMaxX - widthY
    val minY = absMinY + widthX
    val maxY = absMaxY - widthY
    
    val boxes = Range(0, numBoxes).map{ b =>
      val wx = Random.nextDouble * widthX + minWidthX
      val wy = Random.nextDouble * widthY + minWidthY
      val cx = minX + Random.nextDouble * (maxX-minX)
      val cy = minY + Random.nextDouble * (maxY-minY)
      val box = new Box(cx-wx, cx+wx, cy-wy, cy+wy)
      for (i <- Range(0, N)) {
        box.addData(dataX(i), dataY(i))
      }
      box.setDomainVolume(absMinX, absMaxX, absMinY, absMaxY)
      box
    }
    // println(boxes)
    
    val gridLinesX = boxes.foldLeft(Set(absMinX, absMaxX)){ case (set, box) => set + box.x1 + box.x2 }.toArray.sorted
    val gridLinesY = boxes.foldLeft(Set(absMinY, absMaxY)){ case (set, box) => set + box.y1 + box.y2 }.toArray.sorted

    val densXY = Array.tabulate(gridLinesX.length - 1, gridLinesY.length - 1){ (i,j) =>
      val x = 0.5 * (gridLinesX(i) + gridLinesX(i+1))
      val y = 0.5 * (gridLinesY(j) + gridLinesY(j+1))
      val densities = boxes.map(b => b.getDensityAt(x, y))
      //println(f"densities at ($x%.3f, $y%.3f): " + densities)
      //densities.reduce(_ * _)
      densities.reduce(_ + _)
    }
    
    val densXYSum = densXY.flatten.reduce(_ + _)
    //println("summary of densities = %10.6f".format(densXYSum))

    val densX = densXY.map(row => row.reduce(_ + _) / densXYSum)
    val densY = densXY.transpose.map(row => row.reduce(_ + _) / densXYSum)
    
    assert(densX.sum ~= 1.0)
    assert(densY.sum ~= 1.0)
    
    var mi = 0.0
    val log2 = math.log(2)
    for (i <- Range(0, gridLinesX.length - 1)) {
      for (j <- Range(0, gridLinesY.length - 1)) {
        val pXY = densXY(i)(j) / densXYSum // only densXY is not yet normalized
        val pX = densX(i)
        val pY = densY(j)
        //println(pXY, pX, pY, pXY * math.log(pXY / (pX*pY))/log2)
        if (pXY != 0) {
          mi += pXY * math.log(pXY / (pX*pY))/log2
        }
      }
    }
    // MatPlotLib.plotGridDensityWithScatter(gridLinesX, gridLinesY, densXY.map(_.map(x => x / densXYSum)), dataX, dataY)
    
    //Helpers.pause()
    mi    
  }
}