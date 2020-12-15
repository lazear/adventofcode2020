structure Day1 : PUZZLE =
struct
  type input = string list
  val day = "1"
  val tests = ("514579", "241861950")
  val input = Utils.read

  fun part1 strs = 
    let
      val nums = map (Option.valOf o Int.fromString) strs
      val res = List.filter (fn (x,y) => x + y = 2020) (Utils.combinations nums)
    in 
      case res
        of [(x, y)] => Int.toString (x * y)
         | _ => raise (Fail "incorrect number of matches!")
    end

  fun combo3 [] = []
    | combo3 (x::xs) = map (fn (a,b) => (x,a,b)) (Utils.combinations xs) @ combo3 xs
  
  fun part2 strs = 
    let
      val nums = map (Option.valOf o Int.fromString) strs
      val res = List.filter (fn (x,y,z) => x + y + z = 2020) (combo3 nums)
    in 
      case res
        of [(x, y, z)] => Int.toString (x * y * z)
         | _ => raise (Fail "incorrect number of matches!")
    end
end

structure D1 = Advent(Day1)