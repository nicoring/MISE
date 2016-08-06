package dmf



// ----------------------------------------------------------------
// Trait specialization for Double 
// ----------------------------------------------------------------

trait DoubleMatrix {
  
  // basic properties
  def numRows: Int
  def numCols: Int
  
  // ranges to facilitate looping
  def rowRange = Range(0, numRows)
  def colRange = Range(0, numCols)
  
  // basic access
  def apply(i: Int, j: Int): Double
  def update(i: Int, j: Int, x: Double): Unit

  // direct slices
  def row(i: Int) = new RowSeq(i)
  def col(j: Int) = new ColSeq(j)
  
  // copy slices
  def rowCopy(i: Int): Array[Double] = Array.tabulate(numCols)(j => this(i,j))
  def colCopy(j: Int): Array[Double] = Array.tabulate(numRows)(i => this(i,j))

  // misc
  def submatrix(selRows: Array[Int], selCols: Array[Int]): DoubleMatrix = {
    return new ViewDoubleMatrix(this, selRows, selCols)
  }
  override def clone() = Matrix.createDoubleMatrix(numRows, numCols, {(i,j) => this(i,j)})

  def normalizeMinMax() {
    for (j <- Range(0, numCols)) {
      
      var maxV = Double.MinValue
      var minV = Double.MaxValue
      
      for (i <- Range(0, numRows)) {
        minV = this(i,j) min minV
        maxV = this(i,j) max maxV
      }
      for (i <- Range(0, numRows)) {
        if (maxV > minV)
          this(i,j) = (this(i,j) - minV) / (maxV - minV)
        else
          this(i,j) = (this(i,j) - minV)
      }
    }
  }


  override def toString: String = {
    var r = ""
    for (i <- 0 until numRows) {
      r += row(i).map(x => x.toString()).mkString("\t") + "\n"
    }
    r
  }
  
  def toString(fmtString: String): String = {
    var r = ""
    for (i <- 0 until numRows) {
      r += row(i).map(x => fmtString.format(x)).mkString("\t") + "\n"
    }
    r
  }
  

  
  // helper classes for slicing
  class RowSeq(i: Int) extends Seq[Double] {
    def apply(j: Int) = DoubleMatrix.this(i,j)
    def length = numCols
    def iterator = {
      new Iterator[Double] {
        private var j = 0
        def hasNext = j < numCols
        def next = {
          val oldJ = j
          j += 1
          DoubleMatrix.this(i,oldJ)
        }
      }
    }
  }

  class ColSeq(j: Int) extends Seq[Double] {
    def apply(i: Int) = DoubleMatrix.this(i,j)
    def length = numRows
    def iterator = {
      new Iterator[Double] {
        private var i = 0
        def hasNext = i < numRows
        def next = {
          val oldI = i
          i += 1
          DoubleMatrix.this(oldI,j)
        }
      }
    }
  }

}


// ----------------------------------------------------------------
// Concrete classes for Double 
// ----------------------------------------------------------------

class BasicDoubleMatrix(rows: Int, cols: Int, initFunc: (Int, Int) => Double) extends DoubleMatrix with Serializable {

  def this() = this(0, 0, (i,j) => 0.0) 
  // println("created matrix with " + rows + "x" + cols)
  // alternative constructors
  // def this(rows: Int, cols: Int) = this(rows, cols, ((i:Int, j:Int) => 0.))

  // internal data structure
  val data = Array.tabulate(rows, cols)((i,j) => initFunc(i,j))

  // basic properties
  def numRows = rows
  def numCols = cols

  // basic access
  def apply(i: Int, j: Int): Double = data(i)(j)
  def update(i: Int, j: Int, x: Double): Unit = { data(i)(j) = x }
  
}

class ViewDoubleMatrix(m: DoubleMatrix, selRows: Array[Int], selCols: Array[Int]) extends DoubleMatrix with Serializable {

  def numRows = selRows.length
  def numCols = selCols.length
  
  def apply(i: Int, j: Int): Double = m(selRows(i), selCols(j))
  def update(i: Int, j: Int, x: Double): Unit = { m(selRows(i), selCols(j)) = x }
  
}

class ViewDoubleMatrixColOnly(m: DoubleMatrix, selCols: Array[Int]) extends DoubleMatrix with Serializable {

  def numRows = m.numRows
  def numCols = selCols.length
  
  def apply(i: Int, j: Int): Double = m(i, selCols(j))
  def update(i: Int, j: Int, x: Double): Unit = { m(i, selCols(j)) = x }
  
}