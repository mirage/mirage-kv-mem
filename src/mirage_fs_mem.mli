module Pure : sig
  module M : Map.S with type key = string

  type t = Cstruct.t M.t
  val empty : unit -> t
  val read : t -> string -> int -> int -> Cstruct.t list
  val size : t -> string -> int64
  val create : t -> string -> t
  val mkdir : t -> string -> t
  val destroy : t -> string -> t
  val stat : t -> string -> Mirage_fs.stat
  val listdir : t -> string -> string list
  val write : t -> string -> int -> Cstruct.t -> t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

(*include Mirage_fs_lwt.S with type t = Cstruct.t M.t*)
