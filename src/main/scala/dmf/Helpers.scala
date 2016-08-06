package dmf

import java.io.PrintStream
import java.io.OutputStream
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import scalax.file.Path
import Implicits._
import java.text.NumberFormat
import java.util.Locale
import scala.util.matching.Regex
import java.io.File


class DmfException(val function: String, val excType: String, val details: String) extends Exception {
  override def toString = "[%s] %s -- %s".format(function, excType, details) 
}


object Implicits {
  
  implicit def Seq2SeqWithMean[T](s: Seq[T])    (implicit num: Numeric[T]) = new SeqWithMean(s)(num)
  implicit def Arr2SeqWithMean[T](arr: Array[T])(implicit num: Numeric[T]) = new SeqWithMean(arr.toSeq)(num)
  
  implicit def Arr2NormalizableArray(arr: Array[Double]) = new NormalizeableArray(arr)

  /** convenience to overcome Scala xml limitation that attributes always must be strings
   *  with this implicit conversion it is simply possible to write <data attribute={someVariable} /> instead of repeated of someVariable.toString
   */
  implicit def anyToText(a: AnyVal) = xml.Text(a.toString)

}


object Helpers {

  // --------------------------------------------------
  // Output Stuff
  // --------------------------------------------------
  
  def outputStdOut: PrintStream = System.out
  
  def outputFile(name: String): PrintStream = new PrintStream(new FileOutputStream(name))
  
  def outputFile(path: Path): PrintStream = new PrintStream(new FileOutputStream(path.path))
  
  def outputFile(file: File): PrintStream = new PrintStream(new FileOutputStream(file))
  
  def outputDummy: PrintStream = new PrintStream(new OutputStream() {
        override def close() {}
        override def flush() {}
        override def write(b: Array[Byte]) {}
        override def write(b: Array[Byte], off: Int, len: Int) {}
        override def write(b: Int) {}
    } );

  def appendToFile(filePath: Path, text: String) { 
    appendToFile(filePath.path, text) 
  }
  def appendToFile(fileName: String, text: String) {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(fileName, true)))
    out.print(text)
    out.close()
  }
  
  def ensureDirectoryExists(fileName: String) {
    Helpers.runProgramThroughBash(f"mkdir -p ${"\""}$fileName${"\""}", false)
  }

  def ensureParentDirectoryExists(fileName: String) {
    ensureParentDirectoryExists(Path.fromString(fileName))
  }
  def ensureParentDirectoryExists(filePath: Path) {
    filePath.parent.map(_.createDirectory(createParents=true, failIfExists=false))
  }
   
  def withRedirectedStdOut(out: PrintStream)(body: => Unit) {
    val systemOutBackup = System.out
    System.setOut(out)
    body
    System.setOut(systemOutBackup)    
  }
  
  def timestamp: String = {
      val df = new java.text.SimpleDateFormat("yyyy-MM-dd_hh:mm:ss");  
      //df.setTimeZone(java.text.TimeZone.getTimeZone("PST"));  
      return df.format(new java.util.Date());  
  }
  
  def pause() {
    val in = scala.io.StdIn.readLine()
    if (in.trim() == "exit")
      System.exit(0)
  }
  
  def readUserInput[T](question: String, default: T): T = {
    try {
      val s = scala.io.StdIn.readLine(question)
      if (s.trim().length==0) return default
      val ret = default match {
        case default: Int => s.toInt
        case default: Double => s.toDouble
        case default: Boolean => s.toBoolean
        case _ => s
      }
      ret.asInstanceOf[T]
    } catch {
      case e: Exception =>
        println("Error reading input. Using default value (" + default + ").")
        default
    }
  }
  
  /**
   * Like python's glob (list file's recursively).
   * Inspired by: http://stackoverflow.com/a/2638109/1804173
   * f must be a directory,
   * r an regular expression like """.*\.html$""".r 
   */
  def glob(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    if (these == null) {
      return Array()
    } else {
      val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
      good ++ these.filter(_.isDirectory).flatMap(glob(_,r))
    }
  }
  
  
  // --------------------------------------------------
  // Misc computational
  // --------------------------------------------------

  /** Repeat given sequence a number of times
   * Example: 
   * repeat(3)(Array(1,2)) = Array(1, 2, 1, 2, 1, 2)
   */
  def repeat[A](n: Int)(arr: Seq[A]): Seq[A] = { if (n > 1) arr ++ repeat(n-1)(arr) else arr }
  
  def powerSet[A](s: Set[A]) = {
    s.foldLeft(Set(Set.empty[A])) {
      (setset, element) =>
        setset union (setset.map(set => set + element))
    }
  }
  
  def cartesianProduct[A](seqOfPossibilities: Seq[Seq[A]]): Seq[Seq[A]] = {
    def combine(acc: Seq[Seq[A]], set: Seq[A]): Seq[Seq[A]] = for (a <- acc; s <- set) yield {
      a :+ s 
    }
    val expanded = seqOfPossibilities.foldLeft(Seq(Seq[A]()))(combine)
    expanded
  }
  
  def convertPartitionToBitSets(partition: Seq[Int]): Seq[collection.mutable.BitSet] = {
    partition.indices.map { i => 
      val sumBlocksBelow = partition.take(i).sum
      val thisBlock = partition(i)
      val bitset = collection.mutable.BitSet(Range(sumBlocksBelow, sumBlocksBelow+thisBlock) :_*)
      bitset
    }          
  }
  
  def correlation(s1: Seq[Double], s2: Seq[Double]): Double = {
    assert(s1.length == s2.length)
    val N = s1.length
    val m1 = s1.mean
    val m2 = s2.mean
    val sdev1 = s1.sdev
    val sdev2 = s2.sdev
    var sum = 0.0
    for (i <- 0 until N) {
      sum += (s1(i) - m1)*(s2(i) - m2)
    }
    return 1.0 / (N-1) / sdev1 / sdev2 * sum 
  }
  
  /*
  def testMannWhitneyWilcoxon(pop1: Array[Double], pop2: Array[Double]): Double = {
    import jsc.independentsamples.MannWhitneyTest
    import jsc.tests.H1
    val test = new MannWhitneyTest(pop1, pop2, H1.NOT_EQUAL)
    1.0 - test.getSP()
  }

  def testStudentT(pop1: Array[Double], pop2: Array[Double]): Double = {
    import jsc.independentsamples.TwoSampleTtest
    import jsc.tests.H1
    val test = new TwoSampleTtest(pop1, pop2, H1.NOT_EQUAL, false)
    1.0 - test.getSP()
  }
  
  def testKolmogorovSmirnov(pop1: Array[Double], pop2: Array[Double]): Double = {
    import jsc.independentsamples.SmirnovTest
    import jsc.tests.H1
    val test = new SmirnovTest(pop1, pop2, H1.NOT_EQUAL)
    1.0 - test.getSP()
  }
  */
  
  // --------------------------------------------------
  // Sort index creation
  // --------------------------------------------------

  def sortIndices[T](seq: Seq[T])(implicit ord: Ordering[T]): Array[Int] = {
    val tosort = Array.tabulate(seq.length)(i => (seq(i), i))
    val sorted = tosort.sortBy(_._1)
    val ret = Array.tabulate(seq.length)(i => sorted(i)._2)
    return ret
  }
  
  def sortRanks[T](seq: Seq[T])(implicit ord: Ordering[T]): Array[Int] = {
    val tosort = Array.tabulate(seq.length)(i => (seq(i), i))
    val sorted = tosort.sortBy(_._1)
    val ret = Array.tabulate(seq.length)(i => sorted.indexWhere(x => x._2 == i))
    return ret
  }  
  
  def sortIndicesAndRanks[T](seq: Seq[T])(implicit ord: Ordering[T]): (Array[Int], Array[Int]) = {
    val tosort = Array.tabulate(seq.length)(i => (seq(i), i))
    val sorted = tosort.sortBy(_._1)
    val ret1 = Array.tabulate(seq.length)(i => sorted(i)._2)
    val ret2 = Array.tabulate(seq.length)(i => sorted.indexWhere(x => x._2 == i))
    return (ret1, ret2)
  }
  
  def precalcSortIndices(m: DoubleMatrix): IntMatrix = {
  
    val indexM = Matrix.createIntMatrix(m.numRows, m.numCols)
    
    for (j <- 0 until m.numCols) {
      val sortedColInd = Range(0, m.numRows).map(i => (m(i,j), i)).sorted.map(_._2)
      for (i <- 0 until m.numRows)
        indexM(i,j) = sortedColInd(i)
    }
    
    indexM
  }
  
  // --------------------------------------------------
  // Matrix related
  // --------------------------------------------------
  
  def calcDistance(dims: Int, seq1: Seq[Double], seq2: Seq[Double], mode: Int): Double = {
    var dist = 0.0
    if (mode == 2) {
      for (d <- 0 until dims) {
        dist += (seq1(d) - seq2(d)) * (seq1(d) - seq2(d))
      }
      dist = math.sqrt(dist)
    }
    else if (mode == 1) {
      for (d <- 0 until dims) {
        dist += math.abs(seq1(d) - seq2(d))
      }
    }
    else if (mode == -1) {
      for (d <- 0 until dims) {
        val newDist = math.abs(seq1(d) - seq2(d))
        if (newDist > dist) {
          dist = newDist
        }
      }
    }
    return dist
  }
  
  // --------------------------------------------------
  // Running external programs
  // --------------------------------------------------

  def runProgramThroughBash(cmd: String, visible: Boolean = false): Int = 
    runProgram(Array("bash", "-c", cmd), visible)
  
  def runProgram(cmd: String, visible: Boolean): Int = 
    runProgram(Left(cmd), visible)
  
  def runProgram(cmd: Array[String], visible: Boolean): Int = 
    runProgram(Right(cmd), visible)
  
  def runProgram(cmd: Either[String, Array[String]], visible: Boolean): Int = {
    
    import java.io._
    
    class StreamGobbler(is: InputStream, lineCallback: (String => Unit)) extends Thread {
      override def run() {
        try {
          val br = new BufferedReader(new InputStreamReader(is))
          var line = br.readLine()
          while (line != null) {
            lineCallback(line)
            line = br.readLine()
          }
        } catch {
          case ioe: IOException => ioe.printStackTrace();  
        }
      }
    }
      
    val proc = cmd match {
      case Left(str)  => Runtime.getRuntime().exec(str)
      case Right(arr) => Runtime.getRuntime().exec(arr)
    }
    def printFunc(s: String) = if (visible) println(s) else () 
    val stdoutReader = new StreamGobbler(proc.getInputStream(), printFunc);
    val stderrReader = new StreamGobbler(proc.getErrorStream(), printFunc);
 
    stdoutReader.start()
    stderrReader.start()
    
    val exitVal = proc.waitFor();
    return exitVal
  }
  
  // --------------------------------------------------
  // Serialization
  // --------------------------------------------------
  
  def serializationSaveToFile(fn: String, o: Any) {
    import java.io._
    import java.util.zip._
    val fos = new FileOutputStream(fn)
    val gzs = new GZIPOutputStream(fos)
    val oos = new ObjectOutputStream(gzs)
    oos.writeObject(o)
    oos.close()
  }
  
  def serializationLoadFromFile(fn: String): Any = {
    import java.io._
    import java.util.zip._
    val fis = new FileInputStream(fn)
    val gzs = new GZIPInputStream(fis)
    val ois = new ObjectInputStream(gzs)
    return ois.readObject()
  }
  
  // --------------------------------------------------
  // Misc Ranges and Linspace (like numpy)
  // --------------------------------------------------
  
  /** 
   * creates [min, ..., max] with a length of numSteps; 
   * stepsize is chosen appropriately;
   * min and max are inclusive (to get non include simple use "drop")
   */  
  def Linspace(min: Double, max: Double, numSteps: Int) = {
    min to max by (max-min)/(numSteps-1)
  }
  
  def Logspace(min: Double, max: Double, numSteps: Int) = {
    val lmin = math.log10(min)
    val lmax = math.log10(max)
    Linspace(lmin, lmax, numSteps).map(x => math.pow(10, x))
  }  
  
  def linearlySpacedIndex(step: Int) = Stream.iterate(step){ last => 
    last + step 
  }
  
  def exponentiallySpacedIndex(firstStep: Int, factor: Double) = {
    Stream.iterate((firstStep, firstStep.toDouble)){ case (last, lastInc) =>
      val nextInc = lastInc * factor
      val next = (last + nextInc).toInt
      (next, nextInc)
    }.map{ case (last, lastInc) => last }
  }
  
}

// --------------------------------------------------
// Statistical helpers for sequences (mean/variance)
// --------------------------------------------------

class SeqWithMean[T] (s: Seq[T]) (implicit num: Numeric[T]) {
  // http://stackoverflow.com/questions/6188990/writing-a-generic-mean-function-in-scala
  def mean = num.toDouble(s.sum) / s.length
  def vari = {
    val m = mean
    s.map(x => num.toDouble(x) - m).map(x => x*x).sum / (s.length-1)
  }
  def sdev = math.sqrt(vari)
  
  def softMax: Seq[Double] = {
    val tmp = s.map(x => math.exp(num.toDouble(x)))
    val sum = tmp.sum
    return tmp.map(x => x / sum)
  }
  
  def normalizedSeqMinMax: Seq[Double] = {
    val minV = num.toDouble(s.min)
    val maxV = num.toDouble(s.max)
    return s.map(x => (num.toDouble(x) - minV) / (maxV - minV))
  }
  
  def median: Double = {
    val N = s.length
    if (N % 2 == 0) {
      0.5 * ( num.toDouble(s(N/2 - 1)) + num.toDouble(s(N/2)) )
    } else {
      num.toDouble(s(N/2))
    }
  }
  
  def tupleMeanSdev = (mean, sdev)
  
  def trim(n: Int)(implicit ord: Ordering[T]): Seq[T] = s.sorted(ord).drop(n).reverse.drop(n)
}

class NormalizeableArray (a: Array[Double]) {
  import Implicits._
  
  def normalizedSumOne: Array[Double] = {
    val sum = a.sum
    Array.tabulate(a.length)(i => a(i) / sum)
  }

  def normalizedMeanOne: Array[Double] = {
    val mean = a.toSeq.mean
    Array.tabulate(a.length)(i => a(i) / mean)
  }

  def normalizedUnitVariance: Array[Double] = {
    val mean = a.toSeq.mean
    val vari = a.toSeq.vari
    Array.tabulate(a.length)(i => (a(i) - mean) / math.sqrt(vari))
  }
  
  def normalizedUnitVariance(trimBy: Double): Array[Double] = {
    val N = a.length
    val toTrim = (trimBy*N).toInt
    val trimmed = a.sorted.drop(toTrim).dropRight(toTrim)
    val mean = trimmed.toSeq.mean
    val vari = trimmed.toSeq.vari
    Array.tabulate(a.length)(i => (a(i) - mean) / math.sqrt(vari))
  }

  def normalizedSoftMax: Array[Double] = {
    val tmp = a.normalizedUnitVariance
    val arr = Array.tabulate(a.length)(i => math.exp(tmp(i)))
    arr.normalizedSumOne
  }
}

// --------------------------------------------------
// Statistical helpers for iterative mean/variance calculation
// --------------------------------------------------

class IterativeMeanAndVariance() {
  // source: http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance (online variance)
  private var n = 0
  private var lastMean = 0.0
  private var m2 = 0.0
  
  private var minimum = Double.MaxValue
  private var maximum = Double.MinValue
  
  def add(vals: Double*) {
    for (x <- vals) {
      if (!x.isNaN()) {
        n = n + 1
        val delta = x - mean
        lastMean += delta/n
        m2 += delta*(x - mean)
        
        if (x < minimum) minimum = x
        if (x > maximum) maximum = x
      }
    }
  }
  
  def mean    : Double = lastMean
  def vari    : Double = m2 / (n - 1)
  def sdev    : Double = math.sqrt(vari)
  def min     : Double = minimum
  def max     : Double = maximum
  def range   : Double = maximum - minimum
  
  override def toString = f"min: $min%12.3f    avg: $mean%12.3f    max: $max%12.3f    sdev: $sdev%12.3f    [$n entries]"
}

// --------------------------------------------------
// Helper class to model data neighbors
// --------------------------------------------------

case class Neighbor(val index: Int, var dist: Double) {
  override def toString = "N(%d, %f)".format(index, dist)
}


// --------------------------------------------------
// Extended Random
// --------------------------------------------------

object ExtendedRandom { 
  
  import scala.util.Random
  
  def limitedGauss(m: Double, s: Double, l: Double): Double = {
    var x = 0.0
    var d = 0.0
    do {
     x = scala.util.Random.nextGaussian()*s + m
     d = math.abs(x - m)
    } while (d > l)
    x
  }
  
  /**
   * Assumes weights are normalized to one; returns an integer in the range [0, w.length-1]
   */
  def nextIntWeighted(w: Array[Double]): Int = {
    //println(w.sum)
    val x = Random.nextDouble()
    var found = false
    var acc = 0.0
    var i = 0
    while (!found && i < w.length) {
      acc += w(i)
      if (x < acc)
        found = true
      else
        i += 1
    }
    //println("acc = " + acc)
    return i
  }
  
  def generateRandomSubspaceList(subspaceMinDim: Int, subspaceMaxDim: Int, totalDims: Int): List[Int] = {
    var ok = false
    var list = List[Int]()
    while (!ok) {
      val buffer = scala.collection.mutable.ArrayBuffer[Int]()
      while (buffer.sum < totalDims) {
        buffer += scala.util.Random.nextInt(subspaceMaxDim - subspaceMinDim + 1) + subspaceMinDim
      }
      if (buffer.sum == totalDims)
        ok = true
      list = buffer.toList
    }
    return list
  }
  
  def randomSubspace(dim: Int, maxDim: Int): collection.mutable.BitSet = {
    collection.mutable.BitSet(Random.shuffle(Range(0, maxDim).toList).take(dim) :_*)
    // TODO: https://issues.scala-lang.org/browse/SI-6948?page=com.atlassian.jira.plugin.system.issuetabpanels:all-tabpanel
  }
  
  def randomSubspaceFixedProb(p: Double, maxDim: Int): collection.mutable.BitSet = {
    val b = collection.mutable.BitSet()
    for (j <- 0 until maxDim) {
      if (scala.util.Random.nextDouble() < p) {
        b.add(j)
      }
    }
    b
  }
  
}


// --------------------------------------------------
// Stopwatch for Benchmarks
// --------------------------------------------------

class Stopwatch {
  
  val timeConversionConstant = 1.0 / 1000.0 / 1000.0 / 1000.0  
  
  private var startTime: Long = 0
  val all = scala.collection.mutable.ListBuffer[Long]()
  
  def start() {
    startTime = System.nanoTime()
  }
  
  def stop() {
    var stopTime = System.nanoTime()
    all += stopTime - startTime
  }
  
  def lastInSec(): Double = {
    return all.last.toDouble * timeConversionConstant
  }
  def lastInMS(): Double = {
    return all.last.toDouble
  }
  
  def min  = all.map(x => x.toDouble * timeConversionConstant).min
  def max  = all.map(x => x.toDouble * timeConversionConstant).max
  def avg  = all.map(x => x.toDouble * timeConversionConstant).toSeq.mean
  def sdev = all.map(x => x.toDouble * timeConversionConstant).toSeq.sdev
    
  def stats: String = {
    import Implicits._
    val all = this.all.map(x => x.toDouble * timeConversionConstant)
    val mini = all.min
    val maxi = all.max
    val mean = all.toSeq.mean
    val sdev = all.toSeq.sdev
    "mean = %10.3f    sdev = %10.3f    min = %10.3f    max = %10.3f    [%d runs]".format(mean, sdev, mini, maxi, all.length)
  }

  def statsPerExecution(execs: Int): String = {
    import Implicits._
    val all = this.all.map(x => x.toDouble * timeConversionConstant / execs)
    val mini = all.min
    val maxi = all.max
    val mean = all.toSeq.mean
    val sdev = all.toSeq.sdev
    "mean = %10.3f    sdev = %10.3f    min = %10.3f    max = %10.3f    [%d runs]".format(mean, sdev, mini, maxi, all.length)
  }

  def reset() {
    all.clear()
  }
  
  def getTotalTimeInSec = all.sum.toDouble * timeConversionConstant
}


class EstimatedTime(updateInterval: Double) {
  val t1 = System.currentTimeMillis()
  var lastStatusTime = t1
  
  def updateAndOptionallyPrintMessage(percentDone: Double) {
    val t2 = System.currentTimeMillis()
    if (t2 > lastStatusTime + updateInterval*1000) {
      val diffInSec = (t2-t1).toDouble / 1000
      val eta = diffInSec / percentDone - diffInSec
      println(f"percent done: ${percentDone*100}%16.3f %%   estimated time remaining: ${eta/3600}%16.1f h    == ${eta/60}%16.1f min    == ${eta}%16.1f sec    [estimated total = ${diffInSec/percentDone/3600}%10.3f h]")
      lastStatusTime = t2
    }
  }
}


// --------------------------------------------------
// CsvWrite, helps to create csv files in a matrix like way
// --------------------------------------------------

class CsvWriter(val rows: Int, val cols: Int, filename: String, delim: String = ";") {
  
  val data = Array.fill(rows, cols)("")
  
  def update(i: Int, j: Int, s: String) = data(i)(j) = s
  
  def save() {
    val out = Helpers.outputFile(filename)
    val txt = Range(0, rows).map { i =>
      Range(0, cols).map(j => data(i)(j)).mkString(";")
    } mkString("\n")
    out.print(txt)
    out.close()
  }
}

// --------------------------------------------------
// Helper object to get some memory info 
// --------------------------------------------------

object MemoryHelper {
  
  def free()  = Runtime.getRuntime().freeMemory()
  def total() = Runtime.getRuntime().totalMemory()  // will change fom Xms to Xmx over time
  def max()   = Runtime.getRuntime().maxMemory()    // should always correspond to Xmx
  def used()  = total - free
  
  def infoAsString() = f"used: ${NumberFormat.getNumberInstance(Locale.US).format(used)}%15s    " +
                       f"total: ${NumberFormat.getNumberInstance(Locale.US).format(total)}%15s    " +
                       f"free: ${NumberFormat.getNumberInstance(Locale.US).format(free)}%15s    " +
                       f"max: ${NumberFormat.getNumberInstance(Locale.US).format(max)}%15s" 
}









