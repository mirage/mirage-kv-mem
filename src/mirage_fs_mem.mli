type write_error = Mirage_kv.write_error
type error = Mirage_kv.error

module Pure : sig
  type t
  type key = Mirage_kv.Key.t
  val empty : unit -> t
  val get : t -> key -> (string, error) result
  val remove : t -> key -> (t, write_error) result
  val list : t -> key -> ((string * [`Value | `Dictionary]) list, error) result

  val set : t -> key -> string -> (t, write_error) result

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

include Mirage_kv_lwt.RW
  with type write_error := write_error
   and type error := error

val connect : string -> t Lwt.t
val pp : t Fmt.t

val equal : t -> t -> bool
