structure Day1 : PUZZLE =
struct
  val day = "1"

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

  fun run input = 
    let
        val p1 = part1 input
        val p2 = part2 input
    in [p1, p2] end

  fun test () =
    let
      val exp = ["514579", "241861950"]
      val res = run (Utils.read "inputs/1_test.txt")
    in 
      Utils.assert "Day 1 test" exp res 
    end 
end

structure X = Runner(Day1)
val _ = RunAll.register X.run