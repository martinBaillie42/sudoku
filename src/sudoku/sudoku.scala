package sudoku

//import java.io.File
import scala.io.Source._
import javax.swing._
import javax.swing.filechooser._

object sudoku {

  def main(args: Array[String]): Unit = {
    
    val chooser = new JFileChooser()
    val view = chooser.getFileSystemView()
//    val in = Console.readLine("Place the Sudoku text file in the '" + view.getHomeDirectory + "' directory. \nEnter its filename here (minus the 'txt' suffix): ")
//    use file.exist to check if it exists
    val file = fromFile(view.getHomeDirectory + "/" + "sudoku" + ".txt")
    val lines = file.getLines mkString "\n"
    val line = lines filter ("_123456789" contains _ )
    
    val initialValues = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    
    val sudokuGrid = Array.fill(81)(List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))
    
    for (x <- 0 until sudokuGrid.length){
      if (line(x) != '_'){
        sudokuGrid(x) = sudokuGrid(x) filter (_ == line(x).toInt - 48)
        
      }
    }    
    
    for (x <- 0 until sudokuGrid.length){
    	if(sudokuGrid(x).length > 1) {
    	  val start = x - (x % 9)
    	  val end = start + 9
//    	  remove all numbers corresponding to the known numbers horizontally
    	  for (y <- start until end) {
    	    if(sudokuGrid(y).length == 1) {
    	    	sudokuGrid(x) = sudokuGrid(x) filter (_ != sudokuGrid(y).apply(0))
    	    }
    	  }
    	  
//    	  remove all numbers corresponding to the known numbers vertically
    	  val verStart = x % 9
    	  for (z <- verStart until sudokuGrid.length if (z - verStart) % 9 == 0 ) {
    		  if(sudokuGrid(z).length == 1) {
    	    	sudokuGrid(x) = sudokuGrid(x) filter (_ != sudokuGrid(z).apply(0))
    		  }
    	  }
    	  
//    	  horizontal = x - (x % 3)
//    	  
    	  
    	  
    	}
    }      
    
    sudokuGrid foreach (println(_))
    
    
  }

}

// http://docs.scala-lang.org/tutorials/scala-for-java-programmers.html
//    val file = new File(view.getHomeDirectory + "/" + in + ".txt")
//    val file = new File(view.getHomeDirectory + "/" + "sudoku" + ".txt")
//    println(file.length)
//    http://www.scala-lang.org/api/current/index.html#scala.io.Source
//    val sudokuGrid = new Array[List[String]](81)
//    val sudokuGrid = Array.fill(81)(List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9))