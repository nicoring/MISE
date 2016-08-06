package dmf

import scala.io.Source
import java.io.File


class ArffReader(filename: String) {
  
  private val lines = Source.fromFile(filename).getLines.toArray
 
  val numAttr = lines.filter(x => x.startsWith("@attribute")).length

  val attrNames = lines.filter(x => x.startsWith("@attribute")).map(line => line.split(" ")(1))
  
  private val linesData = lines.drop(lines.indexWhere(x => x == "@data") + 1).filter(x => x.split(",").length == numAttr)

  val numPatt = linesData.length

  val matrix = Matrix.createDoubleMatrix(numPatt, numAttr)

  var i = 0
  for (line <- linesData) {
    val fields = line.split(",")
    var j = 0
    for (el <- fields) {
      matrix(i,j) = el.toFloat
      j += 1
    }
    i += 1
  }

  //println(matrix.toStr())
}


object MiscIO {
  
  def loadCSV(fn: File, hasHeader: Boolean = false, separator: String = ";"): DoubleMatrix = {
    val src = Source.fromFile(fn)
    val nDrop = if (hasHeader) 1 else 0
    val rows = src.getLines().drop(nDrop).map(_.split(separator)).toArray
    
    if (rows.length == 0)
      return Matrix.createDoubleMatrix(0, 0)
    
    val numCols = rows(1).length
    if (numCols == 0)
      return Matrix.createDoubleMatrix(0, 0)
    
    val filteredRows = rows.filter(_.length == numCols)
    
    def safeToDouble(x: String) = try x.toDouble catch { case e: NumberFormatException => Double.NaN } 
    
    val m = Matrix.createDoubleMatrix(filteredRows.length, numCols, (i,j) => safeToDouble(filteredRows(i)(j)))
      
    m
  }
  
  def saveCSV(m: DoubleMatrix, optTarget: Option[Array[Double]], fn: String) {
    
    val out = Helpers.outputFile(fn)
    
    optTarget match {
      case None =>
        Range(0, m.numRows).foreach(i => out.println(m.row(i).mkString(", ")))
      case Some(target) =>
        assert(target.length == m.numRows)
        Range(0, m.numRows).foreach(i => out.println((m.row(i).toSeq :+ target(i)).mkString(", ")))
    }
    
  }

  
  def saveArff(m: DoubleMatrix, optTarget: Option[Array[Double]], fn: String) {
  
    val out = Helpers.outputFile(fn)
    
    out.println("@relation data")
    
    Range(0, m.numCols).foreach(j => out.println("@attribute attr%03d real".format(j)))
    
    optTarget match {
      case None =>
        out.println("@data")
        Range(0, m.numRows).foreach(i => out.println(m.row(i).mkString(", ")))
      case Some(target) =>
        assert(target.length == m.numRows)
        out.println("@attribute class " + target.toSet.toList.sorted.mkString("{", ", ", "}"))
        out.println("@data")
        Range(0, m.numRows).foreach(i => out.println((m.row(i).toSeq :+ target(i)).mkString(", ")))
    }
    
  }

  def loadMatrix(fn: String): DoubleMatrix = {
    val reader = new ArffReader(fn)
    val num_rows = reader.matrix.numRows
    val num_cols = reader.matrix.numCols
    return reader.matrix
  }
  
  def loadMatrixWithAttributeNames(fn: String): (DoubleMatrix, Array[String]) = {
    val reader = new ArffReader(fn)
    val num_rows = reader.matrix.numRows
    val num_cols = reader.matrix.numCols
    return (reader.matrix, reader.attrNames)
  }
  
  def loadMatrixWithTarget(fn: String): (DoubleMatrix, Array[Double]) = {
    val reader = new ArffReader(fn)
    val num_rows = reader.matrix.numRows
    val num_cols = reader.matrix.numCols
    val matrix = reader.matrix.submatrix((0 until num_rows).toArray, (0 until num_cols-1).toArray).clone()
    val target = reader.matrix.colCopy(num_cols-1)
    return (matrix, target)
  }
  
}