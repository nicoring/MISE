package dmf.stream.mutinf


import scala.util.Random
import dmf.Helpers
import dmf.SortedBufferKeepLow
import dmf.Stopwatch
import dmf.Implicits._


trait NeighborSummary {
  def addNeighbor(neighborX: Double, neighborY: Double)
  def getNeighborhood(N: Int): (Int, Int, Double)
}



class IncrementalNeighborSummary(val x: Double, val y: Double, k: Int) extends NeighborSummary {
  
  val nearestNeighbors = new SortedBufferKeepLow[Double](k)
  val xNeighbors = new collection.mutable.PriorityQueue[(Double, Int)]()
  val yNeighbors = new collection.mutable.PriorityQueue[(Double, Int)]()
  
  val kNeighborDists = new collection.mutable.ArrayBuffer[(Int, Double)]()
  kNeighborDists += ((0, Double.MaxValue))
  var t = 0
  
  def addNeighbor(neighborX: Double, neighborY: Double) {
    val xDist = math.abs(x-neighborX)
    val yDist = math.abs(y-neighborY)
    val dist =  xDist max yDist
    if (nearestNeighbors.add(dist) && nearestNeighbors.length == k) {
      kNeighborDists += ((t, nearestNeighbors.max))
    }
    if (xDist < kNeighborDists.last._2) {
      xNeighbors.enqueue((xDist, t))
    }
    if (yDist < kNeighborDists.last._2) {
      yNeighbors.enqueue((yDist, t))
    }
    //println(f"after adding neighbor with dist $dist%10.6f    kNeighborDist = ${kNeighborDists.last._2}%10.6f    ${nearestNeighbors.sortedList}    ${xNeighbors.toList.sorted}    ${yNeighbors.toList.sorted}")
    t += 1
  }
  
  def getNeighborhood(N: Int): (Int, Int, Double) = {
    if (N < k)
      return (0, 0, 0.0)
    else {
      val kNeighborDist = kNeighborDists.filter(_._1 < N).last._2
      val nx = xNeighbors.toArray.filter(n => n._1 < kNeighborDist && n._2 < N).length      // check: whether equal is correct wrt N
      val ny = yNeighbors.toArray.filter(n => n._1 < kNeighborDist && n._2 < N).length      // check: whether equal is correct wrt N
      //println(f"kNeighborDist = $kNeighborDist%10.6f    ${xNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}    ${yNeighbors.toList.filter(n => n._1 < kNeighborDist && n._2 < N)}")
      return (nx, ny, kNeighborDist)
    }
  }
  
  def printInfo() {
    println(f"k nearest neighbor changes: ${kNeighborDists}")
  }
}


class TrivialNeighborSummary(val x: Double, val y: Double, k: Int) extends NeighborSummary {
  
  val data = new collection.mutable.ArrayBuffer[(Double, Double)]()
  
  def addNeighbor(neighborX: Double, neighborY: Double) {
    data += ((neighborX, neighborY))
  }
  
  def getNeighborhood(N: Int): (Int, Int, Double) = {
    if (N < k)
      return (0, 0, 0.0)
    else {
      val sample = data.take(N)
      val distances = sample.map(p => math.abs(x-p._1) max math.abs(y-p._2)).sorted
      val kNeighborDist = distances(k-1)
      val nx = sample.filter(p => math.abs(x-p._1) < kNeighborDist).length
      val ny = sample.filter(p => math.abs(x-p._2) < kNeighborDist).length
      //println(f"kNeighborDist = $kNeighborDist%10.6f")
      return (nx, ny, kNeighborDist)
    }
  }
  
}



object TestNeighborSummary {
  
  def main(args: Array[String]) {
    
    //systematicTest()
    //plotEvolvement()
    testAsymptoticBehavior()
  }
  
  
  def debugTest() {
    val N = Helpers.readUserInput("N = ", 1000)
    val k = Helpers.readUserInput("k = ", 5)
    val numberQueries = Helpers.readUserInput("numberQueries = ", 100)

    val ns1 = new TrivialNeighborSummary(0.5, 0.5, k)
    val ns2 = new IncrementalNeighborSummary(0.5, 0.5, k)
    val summaries = List(ns1, ns2)
    
    val data = Array.tabulate(N)(_ => (Random.nextDouble, Random.nextDouble))
    
    println("\n *** filling summaries:")
    data.foreach { case (x, y) =>
      summaries.foreach(_.addNeighbor(x, y))
    }
    
    ns2.printInfo()
    
    println("\n *** query summaries:")
    for (queryLength <- Range(0, N+1)) { // Range(0, numberQueries)
      //val queryLength = Random.nextInt(data.length)
      val results = summaries.map(_.getNeighborhood(queryLength))
      println(f"query length = $queryLength%5d    $results")
      assert(results.toSet.size == 1)
    }
  }
  

  def systematicTest() {
    val N = 1000
    val iterations = 20
    val kValues = List(1, 10, 20)
    
    val stopwatchFill1 = new Stopwatch
    val stopwatchFill2 = new Stopwatch
    val stopwatchQuery1 = new Stopwatch
    val stopwatchQuery2 = new Stopwatch
    
    for (iter <- Range(0, iterations)) {
      for (k <- kValues) {
        println(f"Iteration ${iter+1} with k = $k")
        
        // build test data
        val data = Array.tabulate(N)(_ => (Random.nextDouble, Random.nextDouble))
        
        // build summaries
        val ns1 = new TrivialNeighborSummary(0.5, 0.5, k)
        val ns2 = new IncrementalNeighborSummary(0.5, 0.5, k)
        val summaries = List((ns1, stopwatchFill1, stopwatchQuery1), (ns2, stopwatchFill2, stopwatchQuery2))
        summaries.foreach{ case (summary, swFill, swQuery) =>
          swFill.start()
          data.foreach { case (x, y) =>
            summary.addNeighbor(x, y)
          }
          swFill.stop()
        }
      
        // run queries
        for (queryLength <- Range(0, N+1)) {
          val results = summaries.map{ case (summary, swFill, swQuery) =>
            swQuery.start()
            val result = summary.getNeighborhood(queryLength)
            swQuery.stop()
            result
          }
          assert(results.length==2 && results.toSet.size==1)
        }        
      }
    }
    
    println(f"Fill times for summary1:  ${stopwatchFill1.getTotalTimeInSec / iterations / N}%12.9f sec/fill  == ${1.0 / (stopwatchFill1.getTotalTimeInSec / iterations / N)}%12.3f fills/sec")
    println(f"Fill times for summary2:  ${stopwatchFill2.getTotalTimeInSec / iterations / N}%12.9f sec/fill  == ${1.0 / (stopwatchFill2.getTotalTimeInSec / iterations / N)}%12.3f fills/sec")
    
    println(f"Query times for summary1: ${stopwatchQuery1.getTotalTimeInSec / iterations / (N+1)}%12.9f sec/query == ${1.0 / (stopwatchQuery1.getTotalTimeInSec / iterations / (N+1))}%12.3f queries/sec")
    println(f"Query times for summary2: ${stopwatchQuery2.getTotalTimeInSec / iterations / (N+1)}%12.9f sec/query == ${1.0 / (stopwatchQuery2.getTotalTimeInSec / iterations / (N+1))}%12.3f queries/sec")
  }
  
  
  
  def plotEvolvement() {
    val N = Helpers.readUserInput("N = ", 1000)
    val k = Helpers.readUserInput("k = ", 5)    
    
    val ns = new TrivialNeighborSummary(0.5, 0.5, k)
    
    val data = Array.tabulate(N)(_ => (Random.nextDouble, Random.nextDouble))
    
    data.foreach { case (x, y) =>
      ns.addNeighbor(x, y)
    }
    
    val results = Range(0, N+1).toArray.map(queryLength => ns.getNeighborhood(queryLength))
    val nx = results.map(_._1) 
    val ny = results.map(_._2) 
    val kNeighborDist = results.map(_._3) 
    
    for (i <- Range(0, N+1)) {
      println(f"${nx(i)}%4d    ${ny(i)}%4d    ${kNeighborDist(i)}%10.6f")
    }
    // MatPlotLib.plotSeries(Range(0, N+1).toArray.map(_.toDouble), Array(nx.map(_.toDouble), ny.map(_.toDouble), kNeighborDist))
    
    // MatPlotLib.plotLines(Array(("nx", nx.indices.toArray.map(i => (i.toDouble, nx(i).toDouble)))))
  }
  
  
  
  def testAsymptoticBehavior() {
    val Ns = List(100, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000)
    val iterations = 50 // 50 dauert etwa 18h, aber leider immernoch recht starke Fluktuationen... vllt klappt es mit trim()
    
    val data = for (N <- Ns) yield {
      val nxValues = for (iter <- Range(0, iterations)) yield {
        val data = Array.tabulate(N)(_ => (Random.nextDouble, Random.nextDouble))
        val ns = new TrivialNeighborSummary(0.5, 0.5, 1)
        data.foreach { case (x, y) => ns.addNeighbor(x, y) }
        val (nx, ny, kDist) = ns.getNeighborhood(N)
        nx
      }
      (N.toDouble, nxValues.trim(5).mean)
    }
    
    val plotData = Array(("nx", data.toArray))
    
    // MatPlotLib.plotLines(plotData)
  }

  
}



