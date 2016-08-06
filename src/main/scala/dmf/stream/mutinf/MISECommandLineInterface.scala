package dmf.stream.mutinf


object MISECommandLineInterface {
  
  val usage = 
"""Usage: java -jar mise.jar [--k <INT>] [--reservoir-size <INT>] [fixed|dynamic]
Runs an instance of MISE.
Communication (inserts/queries) is performed via STDIN/STDOUT.
    
Arguments:
  fixed                     runs the MISE version with a fixed reservoir (default)
  dynamic                   runs the MISE version with a dynamic reservoir
  --k <INT>                 specifies the k used to determine kNND (default 1)
  --reservor-size <INT>     specifies either alpha or S depending on whether
                            the dynamic or fixed version is used (default 100)
  --help                    displays help text
"""
    
  def main(args: Array[String]) {

    type OptionMap = Map[Symbol, Any]

    def nextOption(map: OptionMap, argList: List[String]) : OptionMap = {
      def isSwitch(s: String) = (s(0) == '-')
      argList match {
        case Nil => map
        case "--k" :: value :: tail => 
          nextOption(map + ('k -> value.toInt), tail)
        case "--reservoir-size" :: value :: tail => 
          nextOption(map + ('rSize -> value.toInt), tail)
        case string :: tail if string == "fixed" => 
          nextOption(map + ('dynamic -> false), tail)
        case string :: tail if string == "dynamic" => 
          nextOption(map + ('dynamic -> true), tail)
        case "--help" :: tail => 
          System.err.println(usage) 
          sys.exit(1) 
        case option :: tail => 
          System.err.println("Unknown option " + option) 
          sys.exit(1) 
      }
    }
    val defaultOptions = Map('k -> 1, 'rSize -> 100, 'dynamic -> false)
    val options = nextOption(defaultOptions, args.toList)

    //System.err.println("Command line: " + args.mkString("[", ", ", "]"))
    System.err.println(usage)
    System.err.println()
    
    System.err.println("Running MISE with options")
    System.err.println(f"    k = ${options('k)}")
    val estimator = if (options('dynamic).asInstanceOf[Boolean]) {
      System.err.println(f"    alpha = ${options('rSize)}    (MISE version with dynamic reservoir)")
      new EstimatorStreamMISEv3Factory(options('k).asInstanceOf[Int], "MS_D", options('rSize).asInstanceOf[Int]).build()
    } else {
      System.err.println(f"    S = ${options('rSize)}    (MISE version with fixed reservoir)")
      new EstimatorStreamMISEv3Factory(options('k).asInstanceOf[Int], "MS_F", options('rSize).asInstanceOf[Int]).build()
    }

    System.err.println()
    System.err.println("Syntax of possible commands via STDIN:\n")
    System.err.println("    insert <X-AS-DOUBLE> <Y-AS-DOUBLE>           Inserts a new X/Y data point into MISE")
    System.err.println("    query <NUMDROP-AS-LONG> <NUMTAKE-AS-LONG>    Queries MISE for the given window using reverse-drop-take-semantic, i.e.,")
    System.err.println("                                                 NUMDROP corresponds to window offset")
    System.err.println("                                                 NUMTAKE corresponds to window size")
    System.err.println("                                                 Returns the mutual information and")
    System.err.println("                                                 the number of query anchors in the window on STDOUT")
    System.err.println("    length                                       Returns total length of stream inserted")
    System.err.println("    quit                                         exits MISE (alternatively send EOF)")
    System.err.println()
    System.err.println("Reading commands from STDIN...")

    Iterator.continually(scala.io.StdIn.readLine)
            .takeWhile(_ != null)
            .map(_.trim())
            .foreach{ line =>
      // System.err.println(f"Received line '$line'")              
      val fields = line.split("\\s+").toList
      fields match {
        case "insert" :: x :: y :: tail => { 
          try {
            estimator.addData(x.toDouble, y.toDouble)
          } catch {
            case e: Throwable => System.err.println("could not parse 'insert' syntax")
          }
        }
        case "query" :: drop :: take :: tail => { 
          try {
            val w = WindowSpec.createReverse(estimator.length, drop.toLong, take.toLong)
            val res = estimator.miQuery(w)
            println(res.mi + " " + res.statSize)
          } catch {
            case e: NumberFormatException => System.err.println("could not parse 'query' syntax")
            case e: Throwable => System.err.println("illegal window boundaries") // e.getStackTraceString
          }
        }
        case "length" :: tail => { 
          println(estimator.length)
        }
        case "quit" :: tail => { 
          sys.exit(0)
        }
        case _ => System.err.println("unkown command")
      }
    }
  }
}
