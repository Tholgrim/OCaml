val create : unit -> 'a list
val singleton : 'a -> 'b -> ('a * 'b list) list
val length : 'a list -> int
val size : ('a * 'b list) list -> int
val add : 'a -> 'b -> ('a * 'b list) list -> ('a * 'b list) list
val mem : 'a -> ('a * 'b) list -> bool
val get : 'a -> ('a * 'b list) list -> 'b
val get_all : 'a -> ('a * 'b) list -> 'b
val replace : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
val remove : 'a -> ('a * 'b) list -> ('a * 'b) list
val iter : ('a -> 'b -> 'c) -> ('a * 'b list) list -> unit
val map : ('a -> 'b -> 'b) -> ('a * 'b list) list -> ('a * 'b list) list
val filter : ('a -> 'b -> bool) -> ('a * 'b list) list -> ('a * 'b) list
val memq : 'a -> 'b -> ('a * 'b list) list -> bool
val removeq : 'a -> 'b -> ('a * 'b list) list -> ('a * 'b list) list
val iter_all : ('a -> 'b -> 'c) -> ('a * 'b list) list -> unit
val map_all : ('a -> 'b -> 'c) -> ('a * 'b list) list -> ('a * 'c list) list
val for_all : ('a -> 'b -> bool) -> ('a * 'b list) list -> bool
val exists : ('a -> 'b -> bool) -> ('a * 'b list) list -> bool
val filter_all :
  ('a -> 'b -> bool) -> ('a * 'b list) list -> ('a * 'b list) list
