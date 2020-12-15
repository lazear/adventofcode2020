structure Day6 : PUZZLE =
struct
  val day = "6"
  val tests = ("11", "6")
  type input = string list list

  (* return groups string/path -> string list list *)
  fun input file = 
    let 
      val fields = map Substring.string o Substring.fields (fn c => c = #"\n") o Substring.full
      val read = TextIO.inputAll o TextIO.openIn
      fun go (""::xs) acc = (rev acc :: go xs [])
        | go (x::xs) acc = go xs (x::acc)
        | go [] acc = [rev acc]
    in go ((fields o read) file) [] end;
  
  structure Dict = RedBlackTree(type key=char; val compare=Char.compare)

  fun part1 inputs = 
    let
      fun counts s = Dict.size (foldr (fn (ch, d) => Dict.insert d (ch, ())) Dict.empty (explode (String.concat s)))
    in
      Int.toString (foldr (fn (s, sum) => sum + counts s) 0 inputs)
    end

  fun part2 inputs =
    let
      fun update (p, d) =
        case Dict.lookup d p 
          of SOME v => Dict.insert d (p, v + 1)
           | NONE => Dict.insert d (p, 1)
      fun addGroup group = foldr (fn (person, d) => foldr update d (explode person)) Dict.empty group
      fun search d n = List.length (List.filter (fn x => x = n) (Dict.values d))
    in
      Int.toString (foldr (fn (x,acc) => acc + search (addGroup x) (List.length x)) 0 inputs)
    end
end

structure D6 = Advent(Day6)