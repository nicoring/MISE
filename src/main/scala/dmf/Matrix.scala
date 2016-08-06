package dmf


// ----------------------------------------------------------------
// Factories
// ----------------------------------------------------------------

object Matrix {

  def createDoubleMatrix(rows: Int, cols: Int): DoubleMatrix = 
    new BasicDoubleMatrix(rows, cols, ((i:Int, j:Int) => 0.0))
  def createDoubleMatrix(rows: Int, cols: Int, initFunc: (Int, Int) => Double): DoubleMatrix = 
    new BasicDoubleMatrix(rows, cols, initFunc)
  def createDoubleMatrix(l: List[List[Double]]): DoubleMatrix = {
    assert(l.length > 0)
    assert(l.map(_.length).max == l.map(_.length).min)
    val rows = l.length
    val cols = l.map(_.length).max
    new BasicDoubleMatrix(rows, cols, (i,j) => l(i)(j))
  }
  def joinDoubleMatrices(matrices: DoubleMatrix*) = {
    assert(matrices.map(_.numRows).toSet.size == 1)
    val size = matrices.map(_.numRows).head
    val matrixMap = matrices.flatMap(m => Range(0, m.numCols).map(i => (m,i)))
    createDoubleMatrix(size, matrixMap.length, (i,j) => { 
      val (m, mInd) = matrixMap(j)
      m(i, mInd)
    })
  }

  def createIntMatrix(rows: Int, cols: Int): IntMatrix = 
    new BasicIntMatrix(rows, cols, ((i:Int, j:Int) => 0))
  def createIntMatrix(rows: Int, cols: Int, initFunc: (Int, Int) => Int): IntMatrix = 
    new BasicIntMatrix(rows, cols, initFunc)
  def createIntMatrix(l: List[List[Int]]): IntMatrix = {
    assert(l.length > 0)
    assert(l.map(_.length).max == l.map(_.length).min)
    val rows = l.length
    val cols = l.map(_.length).max
    new BasicIntMatrix(rows, cols, (i,j) => l(i)(j))
  }

}

