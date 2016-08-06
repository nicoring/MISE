package dmf.stream.mutinf


trait EstimatorStatic {
  def calcMI(dataX: Array[Double], dataY: Array[Double]): Double
  def minPossibleQuerySize: Int
}




