structure DayX : PUZZLE =
struct
  type input = string list
  val day = "X"
  val tests = ("not finished", "not finished")
  val input = Utils.read 
  
  fun part1 inputs = "not finished"

  fun part2 inputs = "not finished"
end

structure Dx = Advent(DayX)