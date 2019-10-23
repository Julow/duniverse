val header : unit Fmt.t

val question_header : unit Fmt.t

val header_indent : unit Fmt.t

val branch : string Fmt.t

val commit : string Fmt.t

val package : Types.Opam.package Fmt.t

val package_name : string Fmt.t

val path : Fpath.t Fmt.t

val good : 'a Fmt.t -> 'a Fmt.t

val bad : 'a Fmt.t -> 'a Fmt.t

val quoted : 'a Fmt.t -> 'a Fmt.t
(** Wrap a formatter in double quotes ("\""). Colored cyan *)

val cached : bool Fmt.t
(** [cached fmt c] formats [" [CACHED]"] if [c] is [true] and formats nothing
    otherwise.
    You should use this to format suffixes of logs that described actions that
    can be cached. *)
