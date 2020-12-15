signature PUZZLE =
sig
  type input
  val day : string
  (* file name to input *)
  val input : string -> input

  val part1 : input -> string
  val part2 : input -> string

  val tests : string * string
end