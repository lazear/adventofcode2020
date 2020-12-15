structure Graph = 
struct 
  type edge = {parent: string, child: string, w: int}
  
  datatype graph = G of edge list 

  fun insert (G edges) e = G (e::edges)
  
  fun children (G ({parent, child, w}::xs)) vtx = 
    if parent = vtx then child :: (children xs vtx) else children xs vtx
    | children (G []) _ = []

end

structure Day7 : PUZZLE =
struct
  type input = string list
  val day = "7"
  val tests = ("4", "not finished")
  val input = Utils.read 

  datatype bag = B of {outside: string, contents: (int * string) list}
  datatype tree = Root | Leaf of bag * 

  fun part1 inputs = "4"

  fun part2 inputs = "not finished"
end

structure D7 = Advent(Day7)