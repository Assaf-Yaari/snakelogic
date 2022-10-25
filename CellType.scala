package snake.logic

sealed abstract class CellType

case class SnakeHead(direction: Direction) extends CellType

case class SnakeBody(distanceToHead: Float = 0f) extends CellType

case class Empty() extends CellType

case class Apple() extends CellType

