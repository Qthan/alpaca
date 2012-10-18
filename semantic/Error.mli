(* Error handling *)

exception Terminate

type verbose = Vquiet | Vnormal | Vverbose

val flagVerbose      : verbose ref

val numErrors        : int ref
val maxErrors        : int ref
val flagWarnings     : bool ref
val numWarnings      : int ref
val maxWarnings      : int ref

val internal_raw     : (string * int) ->
                         ('a, Format.formatter, unit) format -> 'a
val fatal            : ('a, Format.formatter, unit) format -> 'a
val error            : ('a, Format.formatter, unit) format -> 'a
val warning          : ('a, Format.formatter, unit) format -> 'a
val message          : ('a, Format.formatter, unit) format -> 'a

type position

val position_point   : Lexing.position -> position
val position_context : Lexing.position -> Lexing.position -> position
val position_dummy   : position
val print_position   : Format.formatter -> position -> unit
