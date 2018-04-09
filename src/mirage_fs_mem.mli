module Pure : sig
  type t
  val empty : unit -> t
  val read : t -> string -> int -> int -> (Cstruct.t list, Mirage_fs.error) result
  val size : t -> string -> (int64, Mirage_fs.error) result
  val create : t -> string -> (t, Mirage_fs.write_error) result
  val mkdir : t -> string -> (t, Mirage_fs.write_error) result
  val destroy : t -> string -> t
  val stat : t -> string -> (Mirage_fs.stat, Mirage_fs.error) result
  val listdir : t -> string -> string list
  val write : t -> string -> int -> Cstruct.t -> (t, Mirage_fs.write_error) result

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

include Mirage_fs_lwt.S
val connect : string -> t Lwt.t
