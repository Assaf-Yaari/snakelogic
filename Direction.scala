package snake.logic

sealed abstract class Direction {
  def opposite : Direction
}


case class East()   extends Direction  { def opposite : West   = West()  }
case class North()  extends Direction  { def opposite : South  = South() }
case class West()   extends Direction  { def opposite : East   = East()  }
case class South()  extends Direction  { def opposite : North  = North() }