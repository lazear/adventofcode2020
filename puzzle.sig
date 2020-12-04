signature PUZZLE =
sig
  val day : string
  (* file name to input *)
  val input : string -> string list

  val part1 : string list -> string
  val part2 : string list -> string

  val tests : string * string
end