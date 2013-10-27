exception Terminate
type verbose = Vquiet | Vnormal | Vverbose
val flagVerbose : verbose ref
val numErrors : int ref
val maxErrors : int ref
val flagWarnings : bool ref
val numWarnings : int ref
val maxWarnings : int ref
type position =
    PosPoint of Lexing.position
  | PosContext of Lexing.position * Lexing.position
  | PosDummy
val position_point : Lexing.position -> position
val position_context : Lexing.position -> Lexing.position -> position
val position_dummy : position
val print_position : Format.formatter -> position -> unit
val no_out : 'a -> 'b -> 'c -> unit
val no_flush : unit -> unit
val null_formatter : Format.formatter
val internal_raw :
  string * int -> ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
val fatal : ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
val error : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
val warning : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
val message : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
