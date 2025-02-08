type error = Mirage_kv.error

type write_error = Mirage_kv.write_error

module Pure : sig
  type t
  type key = Mirage_kv.Key.t
  val empty : Ptime.t -> unit -> t
  val get : t -> key -> (string, error) result
  val size : t -> key -> (Optint.Int63.t, error) result
  val get_partial : t -> key -> offset:Optint.Int63.t -> length:int -> (string, error) result
  val last_modified : t -> key -> (Ptime.t, error) result
  val remove : t -> key -> Ptime.t -> (t, write_error) result
  val list : t -> key -> ((key * [`Value | `Dictionary]) list, error) result

  val set : t -> key -> Ptime.t -> string -> (t, write_error) result
  val set_partial : t -> key -> Ptime.t -> offset:Optint.Int63.t -> string -> (t, write_error) result
  val rename : t -> source:key -> dest:key -> Ptime.t -> (t, write_error) result

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

include Mirage_kv.RW
  with type write_error := write_error
   and type error := error

val connect : unit -> t Lwt.t
val pp : t Fmt.t
val equal : t -> t -> bool
val set_last_modified : t -> Ptime.t option -> unit
