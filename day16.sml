structure Day16 =
struct
  type input = string list list
  val day = "16"
  val tests = ("71", "not finished")
  val input = Day6.input (* tokenize by double line break *)
  
  type constraint = 
    { a: int * int
    , b: int * int }

  type data = 
    { constraints: constraint list
    , ticket: int list
    , nearby: int list list }
  
  fun parse inputs = 
    let 
      fun tokenize line = List.mapPartial (Int.fromString o Substring.string) (Substring.tokens (not o Char.isDigit) (Substring.full line))

      fun pConstraint (line : string) : constraint = 
        let val xs = tokenize line
            val (a, b) = case xs 
              of [a, a', b, b'] => ((a, a'), (b, b'))
               | _ => raise (Fail "invalid constraint form")
        in {a=a, b=b} end
    in
      case inputs
        of [cons, (_::ticket::nil), (_::nearby)] => {constraints=map pConstraint cons, ticket=tokenize ticket, nearby=map tokenize nearby}
         | _ => raise (Fail "invalid input form")
    end
  
  val sum = foldl op+ 0
  fun valid (a, b) x = x >= a andalso x <= b
  
  fun checkConstraints cs x = if List.exists (fn {a, b} => valid a x orelse valid b x) cs then 0 else x
  fun sumInvalidFields con ticket = sum (map (checkConstraints con) ticket)
  fun countInvalid {constraints, nearby, ticket} = sum (map (sumInvalidFields constraints) nearby)

  fun part1 inputs = Int.toString (countInvalid (parse inputs))


  val inputs = input "inputs/16.txt"
      (* only valid tickets *)
      fun filter c = List.filter (fn tix => sumInvalidFields c tix = 0)

      (* list of invalid column * constraint indices *)
      fun genAssoc cs ((i, v)::tix) = 
          let
            fun go ((j, {a,b})::cs) = if valid a v orelse valid b v then go cs else (i, j)::go cs
              | go [] = []
          in  (go (Utils.enumerate cs)) :: (genAssoc cs tix) end 
        | genAssoc _ _ = []
      
      fun matrix c t = genAssoc c (Utils.enumerate t)

      fun condense n lists = 
        let
          val empty = (List.tabulate (n, fn _ => []))
          fun inner (x::xs) (y::ys) = (x @ y) :: inner xs ys
            | inner [] [] = []
        in 
          List.foldr (fn (xs, acc) => inner xs acc) empty lists
        end
      
      val data = parse inputs
      val c = #constraints data
      val n = #nearby data
      val tickets = #ticket data :: filter c n

      (* matrix of constraints : list list list *)
      val mat = map (fn x => matrix c x) tickets

      val nconstraints = (List.length c)
      (* (column idx, constraint idx) list list  - invalid constraints per column *)
      val mat' = condense nconstraints mat

      (* condense a single column/constraint list down to a list of remaining possiblities *)
      fun simplify n xs = List.mapPartial (fn x => x) (List.tabulate (n, fn x => if List.exists (fn (tidx, cidx) => x = cidx) xs then NONE else SOME x))

    fun part2 inputs = "not finished"

end

structure D16 = Advent(Day16)