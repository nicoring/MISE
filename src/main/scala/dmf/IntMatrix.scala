package dmf



// ----------------------------------------------------------------
// Trait specialization for Int 
// ----------------------------------------------------------------

trait IntMatrix {
  
  // basic properties
  def numRows: Int
  def numCols: Int
  
  // ranges to facilitate looping
  def rowRange = Range(0, numRows)
  def colRange = Range(0, numCols)
  
  // basic access
  def apply(i: Int, j: Int): Int
  def update(i: Int, j: Int, x: Int): Unit

  // direct slices
  def row(i: Int) = new RowSeq(i)
  def col(j: Int) = new ColSeq(j)
  
  // copy slices
  def rowCopy(i: Int): Array[Int] = Array.tabulate(numCols)(j => this(i,j))
  def colCopy(j: Int): Array[Int] = Array.tabulate(numRows)(i => this(i,j))

  // misc
  def submatrix(selRows: Array[Int], selCols: Array[Int]): IntMatrix = {
    return new ViewIntMatrix(this, selRows, selCols)
  }
  override def clone() = Matrix.createIntMatrix(numRows, numCols, {(i,j) => this(i,j)})

  def normalizeMinMax() {
    for (j <- Range(0, numCols)) {
      
      var maxV = Int.MinValue
      var minV = Int.MaxValue
      
      for (i <- Range(0, numRows)) {
        minV = this(i,j) min minV
        maxV = this(i,j) max maxV
      }
      for (i <- Range(0, numRows)) {
        this(i,j) = (this(i,j) - minV) / (maxV - minV) 
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

  
  // helper classes for slicing
  class RowSeq(i: Int) extends Seq[Int] {
    def apply(j: Int) = IntMatrix.this(i,j)
    def length = numCols
    def iterator = {
      new Iterator[Int] {
        private var j = 0
        def hasNext = j < numCols
        def next = {
          val oldJ = j
          j += 1
          IntMatrix.this(i,oldJ)
        }
      }
    }
  }

  class ColSeq(j: Int) extends Seq[Int] {
    def apply(i: Int) = IntMatrix.this(i,j)
    def length = numRows
    def iterator = {
      new Iterator[Int] {
        private var i = 0
        def hasNext = i < numRows
        def next = {
          val oldI = i
          i += 1
          IntMatrix.this(oldI,j)
        }
      }
    }
  }

}


// ----------------------------------------------------------------
// Concrete classes for Int 
// ----------------------------------------------------------------

class BasicIntMatrix(rows: Int, cols: Int, initFunc: (Int, Int) => Int) extends IntMatrix {

  def this() = this(0, 0, (i,j) => 0) 
  // println("created matrix with " + rows + "x" + cols)
  // alternative constructors
  // def this(rows: Int, cols: Int) = this(rows, cols, ((i:Int, j:Int) => 0.))

  // internal data structure
  val data = Array.tabulate(rows, cols)((i,j) => initFunc(i,j))

  // basic properties
  def numRows = rows
  def numCols = cols

  // basic access
  def apply(i: Int, j: Int): Int = data(i)(j)
  def update(i: Int, j: Int, x: Int): Unit = { data(i)(j) = x }
  
}

class ViewIntMatrix(m: IntMatrix, selRows: Array[Int], selCols: Array[Int]) extends IntMatrix {

  def numRows = selRows.length
  def numCols = selCols.length
  
  def apply(i: Int, j: Int): Int = m(selRows(i), selCols(j))
  def update(i: Int, j: Int, x: Int): Unit = { m(selRows(i), selCols(j)) = x }
  
}

class ViewIntMatrixColOnly(m: IntMatrix, selCols: Array[Int]) extends IntMatrix {

  def numRows = m.numRows
  def numCols = selCols.length
  
  def apply(i: Int, j: Int): Int = m(i, selCols(j))
  def update(i: Int, j: Int, x: Int): Unit = { m(i, selCols(j)) = x }
  
}
