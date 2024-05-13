open! Unix
open! Monster

type power_mode =
  | Typical
  | PowerSaving

type hazard =
  | PowerSurge
  | LightMalfunction
  | DoorJam

type game_state

val hourly_messages : string list

val elapsed_time : game_state -> float

val time_is_up : game_state -> bool

val game_hour : game_state -> int

val gen_state : int -> float -> bool -> bool -> hazard option -> bool -> bool -> (int * bool) list -> monster list -> power_mode -> bool -> difficulty -> int -> float list -> game_state

val get_battery : game_state -> int

val get_start_time : game_state -> float

val get_door_closed : game_state -> bool

val get_door_jammed : game_state -> bool

val get_hazard : game_state -> hazard option

val get_light_on : game_state -> bool

val get_light_malfunction : game_state -> bool

val get_camera_statuses : game_state -> (int * bool) list

val get_monsters : game_state -> monster list

val get_power_mode : game_state -> power_mode

val get_generator_on : game_state -> bool

val get_difficulty : game_state -> difficulty

val get_last_announced_hour : game_state -> int

val get_command_times : game_state -> float list

val initial_state : difficulty -> game_state

val update_command_times: game_state -> float -> unit

val too_many_commands: game_state -> float -> bool

val read_ascii_art_from_file : string -> string

val power_consumption_rates : game_state -> string -> int

val toggle_power_mode : game_state -> unit

val operate_generator : game_state -> unit

val toggle_generator : game_state -> unit

val display_random_ascii_art : string list -> unit

val move_monsters_and_check_game_over : game_state -> bool -> bool * string list

val update_camera_statuses : game_state -> unit

val print_map : unit -> unit

val apply_random_power_up : game_state -> unit

val random_hazard : game_state -> unit

val random_events : game_state -> unit

val resolve_hazard : game_state -> unit

val process_command : game_state -> string -> unit

val list_to_string : string list -> string

val game_loop : game_state -> unit

val choose_difficulty : unit -> difficulty

val start_or_tutorial : unit -> unit