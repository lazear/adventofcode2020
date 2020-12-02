structure Utils =
struct
  (* split : char -> string -> string list *)
  fun split ch = map Substring.string o Substring.tokens (fn c => c = ch) o Substring.full

  (* given a filename, return a list of lines *)
  val read = (split #"\n") o TextIO.inputAll o TextIO.openIn
 
  fun combinations [] = []
    | combinations (x::xs) = map (fn a => (x, a)) xs @ (combinations xs)
  
  fun assert s x y = if x = y then () else raise (Fail ("Test failed: " ^ s))
end

structure Calendar =
struct
  val days : (unit -> unit) list ref = ref []

  fun register f = days := (f :: !days)

  fun run () = map (fn x => x ()) (!days)

  fun runLast () = 
    case !days
      of x::_ => x ()
       | [] => ()

end

functor Advent(Day : PUZZLE) = 
struct 
  fun test () = 
    let
      val input = Utils.read ("inputs/" ^ Day.day ^ "_test.txt")
      val _ = Utils.assert ("Day " ^ Day.day ^ " part 1 test") (Day.part1 input) (#1 Day.tests)
      val _ = Utils.assert ("Day " ^ Day.day ^ " part 2 test") (Day.part2 input) (#2 Day.tests)
    in 
      ()
    end 

  fun run () = 
    let
      val fname = "inputs/" ^ Day.day ^ ".txt"
      val input = Utils.read fname
      val _ = test ()
    in 
      (
        print ("Day " ^ Day.day ^ ", Part 1: " ^ Day.part1 input);
        print ("\nDay " ^ Day.day ^ ", Part 2: " ^ Day.part2 input);
        print "\n"
      )
    end
  
  val _ = Calendar.register run
end
