type monster
(** The type of a monster *)

type difficulty =
  | Tutorial
  | Easy
  | Normal
  | Hard
(** The difficulty of the game/monsters, influencing speed of progression *)

val init_monsters : monster list
(** [init_monsters] is a list of four monsters. *)

val get_pace : difficulty -> monster -> bool -> float
(** [get_pace diff name generator_on] is how long it takes monster [name] with 
game difficulty [diff] to move to the next room given generate state 
[generator_on]. *)

val move_monster : monster -> float -> difficulty -> bool -> bool -> bool
(** [move_monster mon time diff door_closed generator_on] modifies the monster 
[mon] to reflect the next state of the monster at time [time] and with game 
difficulty [diff] and with door state [door_closed] and with generator state 
[generator_on] and returns true if monster successfully reaches the office; 
otherwise false. *)

val update_monsters : monster list -> float -> difficulty -> bool -> bool -> bool * string list
(** [update_monsters monsters time diff door_closed gen_on] updates the monster 
locations under conditions time [time], difficulty [difficulty], door state 
[door_closed], and generator state [gen_on] and returns a boolean indicating if 
any monsters successfully reach the office and the list of monster names that 
reach the office. *)

val get_locations : monster list -> int list
(** [get_locations monsters] is a list of the locations of [monsters]. The 
locations are in the same order as the monsters, and duplicates are kept. *)

val get_monsters_at_location : monster list -> int -> string list
(** [get_monsters_at_location monsters loc] is the list of monsters at a certain 
location. *)