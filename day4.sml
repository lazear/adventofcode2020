structure Day4 : PUZZLE =
struct
  type input = string list
  val day = "4"
  val tests = ("2", "2")
 
  fun input file = 
    let 
      val fields = map Substring.string o Substring.fields (fn c => c = #"\n") o Substring.full
      val read = TextIO.inputAll o TextIO.openIn
      fun go (""::xs) acc = (String.concatWith " " (rev acc) :: go xs [])
        | go (x::xs) acc = go xs (x::acc)
        | go [] acc = [String.concat (rev acc)]
    in go ((fields o read) file) [] end;

  fun part1 inputs = 
    let
      val required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      fun fields line = map (hd o (Utils.split #":")) (Utils.split #" " line)
      fun valid line = List.all (fn x => x) (map (fn r => List.exists (fn x => x = r) (fields line)) required)
    in
      Int.toString (Utils.count valid inputs)
    end

  structure Dict = RedBlackTree(type key=string; val compare=String.compare)

  fun part2 inputs = 
    let
      fun extract s =
        case Utils.split #":" s
          of (k::v::[]) => (k, v)
           | _ => ("", "")

      fun fields line = map extract (Utils.split #" " line)

      fun valid_int (min, max) x = 
        case Int.fromString x 
          of SOME n => String.size x = 4 andalso n >= min andalso n <= max
           | NONE => false

      fun valid_height s = 
        let
          val ss = Substring.full s
          val (hgt, ss') = Substring.splitl Char.isDigit ss
          val rest = Substring.string ss'
          fun valid (min, max) x = 
            case Int.fromString x 
              of SOME n => n >= min andalso n <= max
              | NONE => false
        in 
          case rest 
            of "in" => valid (59, 76) (Substring.string hgt)
             | "cm" => valid (150, 193) (Substring.string hgt)
             | _ => false
        end
      
      fun valid_hcl s =
        let 
          val (h, ss) = Substring.splitl (fn c => c = #"#") (Substring.full s)
          val (d, ss) = Substring.splitl Char.isHexDigit ss
        in (Substring.size h) = 1 andalso (Substring.size d) = 6 andalso (Substring.isEmpty ss) end

      fun valid_ecl s =
        List.exists (fn x => x = s) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      
      fun valid_pid s = 
        let 
          val (d, ss) = Substring.splitl Char.isDigit (Substring.full s)
        in (Substring.size d) = 9 andalso (Substring.isEmpty ss) end

      val dict = Dict.empty
      val dict = Dict.insert dict ("byr", valid_int (1920, 2002))
      val dict = Dict.insert dict ("iyr", valid_int (2010, 2020))
      val dict = Dict.insert dict ("eyr", valid_int (2020, 2030))
      val dict = Dict.insert dict ("hgt", valid_height)
      val dict = Dict.insert dict ("hcl", valid_hcl)
      val dict = Dict.insert dict ("ecl", valid_ecl)
      val dict = Dict.insert dict ("pid", valid_pid)
      val dict = Dict.insert dict ("cid", fn _ => true)

      fun validate_field (k, v) = 
        case Dict.lookup dict k
          of SOME f => f v
           | NONE => false 
      
      fun validate_pass kvs = 
        let 
          val a = List.all (fn x => x) (map validate_field kvs)
          val required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
          val b = List.all (fn x => x) (map (fn r => List.exists (fn (x,_) => x = r) kvs) required)
        in a andalso b end
    in
      Int.toString (Utils.count (fn x => x) (map (validate_pass o fields) inputs))
    end
end

structure D4 = Advent(Day4)