package snake.logic
import scala.collection.mutable.ArrayBuffer
import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._
import scala.util.Random
import scala.collection.mutable
import scala.collection.mutable.Stack


case class GameState(val tail : Point , val snakeLength : Int , val appleLocation : Point , val snakeFacing : Direction , val over : Boolean) {}
class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {

  var snakeParts = Vector (Point(2,0) , Point(1,0), Point(0,0))
  var gameOverflag = false
  def gameOver: Boolean = gameOverflag
  var apple = appleGenarate()
  var growthCounter = 0
  var reversFlag = false
  var dir : Direction = East()
  var snakeSteps = Stack[GameState]()
  var gameSteps = GameState (snakeParts.last, snakeParts.length, apple ,dir ,gameOverflag)
  snakeSteps.push(gameSteps)

  def behindSnakehead (s: Direction): Boolean = {
      s match{
      case East() => if((snakeParts(1).x > snakeParts(0).x)) {return true}
      case West () => if((snakeParts(1).x < snakeParts(0).x)) {return true}
      case North () => if((snakeParts(1).y < snakeParts(0).y)) {return true}
      case South () => if((snakeParts(1).y > snakeParts(0).y)){return true}
    }
    return false
  }

  def checkEndgame () : Unit={
    var headCheck = snakeParts.head
    if(snakeParts.tail.contains(headCheck))
      gameOverflag = true
  }

  def appleGenarate (): Point ={
    var freeSpots = ArrayBuffer[Point]()
    for(i <- gridDims.allPointsInside ){
      if(getCellType(i) == Empty())
        freeSpots += i
    }

    if(freeSpots.length > 0)
      freeSpots(random.randomInt(freeSpots.length))
    else
      Point (gridDims.width+1 ,gridDims.height+1)
  }

  def wrapAdound (): Unit ={
  
    var xVal = snakeParts.head.x
    var yval = snakeParts.head.y
    if(xVal < 0 || xVal == gridDims.width)
      xVal = gridDims.width - xVal.abs  
    else if (yval < 0 || yval == gridDims.height)
      yval = gridDims.height - yval.abs  
    snakeParts = snakeParts.updated(0, Point(xVal ,yval))
  }

  def checkappleObtained () : Unit= {
    if(snakeParts.head == apple){
        growthCounter+=1
        apple = appleGenarate()
      }
  }

  def snakeMoves() : Unit = {
      for( i <- snakeParts.length-1 until 0 by -1){
      snakeParts= snakeParts.updated(i ,snakeParts(i-1))
    }
      dir match {
      case East() => snakeParts = snakeParts.updated(0 ,Point(snakeParts.head.x +1 ,snakeParts.head.y))
      case West() => snakeParts = snakeParts.updated(0,Point(snakeParts.head.x -1 ,snakeParts.head.y))
      case South() => snakeParts =  snakeParts.updated(0,Point(snakeParts.head.x  ,snakeParts.head.y +1))
      case North() => snakeParts = snakeParts.updated(0 ,Point(snakeParts.head.x  ,snakeParts.head.y -1))
    }
    wrapAdound()
   }

  def snakeGrow () : Unit={
    if(growthCounter > 0){
       if(snakeParts.length +1 <  gridDims.allPointsInside.length){
        var newtail = snakeParts.last
        snakeParts = snakeParts :+ newtail
      }
      if(snakeParts.length % 3 == 0)
        growthCounter -=1
    }   
}
  def step(): Unit ={
    if(!gameOverflag && !reversFlag  ){     
      snakeGrow()
      snakeMoves()
      checkappleObtained()
      gameSteps = GameState (snakeParts.last, snakeParts.length, apple ,dir ,gameOverflag)
      snakeSteps.push(gameSteps)
      checkEndgame()
    }
    else if(reversFlag && snakeSteps.size > 1){
     reverseMode ()
  }
}

  def setReverse(r: Boolean): Unit = {reversFlag = r}

  def reverseMode () : Unit = {
    snakeSteps.pop
    if(snakeParts.length != snakeSteps.top.snakeLength)
      snakeParts = snakeParts.dropRight(1)  
    for( i <- 0 until snakeParts.length-1)
      snakeParts = snakeParts.updated(i, snakeParts(i+1)) 
    snakeParts = snakeParts.updated (snakeParts.length-1,  snakeSteps.top.tail)
    apple = snakeSteps.top.appleLocation
    dir = snakeSteps.top.snakeFacing
    gameOverflag = snakeSteps.top.over
  }
  def changeDir(d: Direction): Unit ={
      if(behindSnakehead(d) == false)
        dir = d 
  }

  def getCellType(p : Point): CellType = {
    for(i <- snakeParts){
      if(snakeParts.head == p)
        return SnakeHead(dir)
      else if(snakeParts.contains(p))
        return SnakeBody(0)
    }
    if(p == apple && snakeParts.size < gridDims.allPointsInside.length)
      return Apple()
    return Empty()
  }
}



/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 8
  val DrawSizeFactor = 1.0 
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 25, height = 25)  
}
