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

structure RunAll =
struct
  val days : (unit -> unit) list ref = ref []

  fun register f = days := (f :: !days)
  fun run () = map (fn x => x ()) (!days)
end


functor Runner(Day : PUZZLE) = 
struct 
  fun run () = 
    let
      val fname = "inputs/" ^ Day.day ^ ".txt"
      val input = Utils.read fname
      val _ = Day.test ()
      val x = Day.run input
      fun fmt p r = "Day " ^ Day.day ^ ", Part " ^ (Int.toString (p + 1)) ^ ": " ^ r
      val f = List.tabulate (List.length x, fn n => fmt n (List.nth (x, n)))
    in 
      print ((String.concatWith "\n" f) ^ "\n")
    end
  
  val _ = RunAll.register run
end
