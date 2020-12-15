structure Day3 : PUZZLE =
struct
  type input = string list
  val day = "3"
  val tests = ("7", "336")
  val input = Utils.read

  fun extract n (i, s) = String.sub (s, (i * n) mod (String.size s))

  fun part1 inputs = 
    Int.toString (Utils.count (fn c => c = #"#") (map (extract 3) (Utils.enumerate inputs)))

  fun part2 inputs = 
    let 
      fun run (x,y) = 
        let 
          (* select y coordinates of slope *)
          val ls = (List.filter (fn (a, _) => (a mod y) = 0) (Utils.enumerate inputs))
          (* renumber y coordinates in enumerated list *)
          val ls' = map (fn (i,n) => (i div y, n)) ls
          (* extract character from each line *)
          val ls' = map (extract x) ls'
        in Utils.count (fn c => c = #"#") ls' end
      val slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      val trees = map (IntInf.fromInt o run) slopes
    in IntInf.toString (foldl op* 1 trees) end
end

structure D3 = Advent(Day3)