structure Day2 : PUZZLE =
struct
  type input = string list
  val day = "2"
  val tests = ("2", "1")
  val input = Utils.read
  
  type policy = {min: int, max: int, chr: char, pwd: string}
  
  fun andThen (SOME a, SOME b) = SOME (a, b)
    | andThen (_, _) = NONE

  fun parse (p : string) : policy option = 
    let
      val ss = Substring.full p
      val (min, ss') = Substring.splitl Char.isDigit ss
      val ss' = Substring.triml 1 ss'
      val (max, ss') = Substring.splitl Char.isDigit ss'
      val ss' = Substring.dropl Char.isSpace ss'
      val (chr, ss') = Substring.splitl Char.isAlpha ss'
      val ss' = Substring.dropl Char.isSpace ss' 
      
      val min = Int.fromString (Substring.string min)
      val max = Int.fromString (Substring.string max)
      val chr = Substring.first chr
      val pwd = Substring.string ss'
    in
      case andThen (chr, andThen (min, max))
        of SOME(chr, (min, max)) => SOME {min=min, max=max, chr=chr, pwd=pwd}
         | NONE => NONE
    end

  fun part1 inputs = 
    let 
        val policies = List.mapPartial parse inputs
        fun validate {min, max, chr, pwd} = 
          let 
            val count = List.length (List.filter (fn ch => ch = chr) (explode pwd))
          in 
            count >= min andalso count <= max 
          end
    in
      Int.toString (List.length (List.filter validate policies))
    end

  fun part2 inputs = 
    let
      val policies = List.mapPartial parse inputs
      fun xor a b = (a orelse b) andalso not (a andalso b)
      fun validate {min, max, chr, pwd} = 
          xor (String.sub (pwd, min + 1) = chr) (String.sub (pwd, max + 1) = chr)
    in
      Int.toString (List.length (List.filter validate policies))
    end
end

structure D2 = Advent(Day2)