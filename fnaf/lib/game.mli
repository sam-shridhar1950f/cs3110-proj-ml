open! Unix
open! Monster

type power_mode

type hazard

type game_state

val hourly_messages : string list

val elapsed_time : game_state -> float

val time_is_up : game_state -> bool

val game_hour : game_state -> int

val initial_state : difficulty -> game_state

val power_consumption_rates : game_state -> string -> int

val toggle_power_mode : game_state -> unit

val operate_generator : game_state -> unit

val toggle_generator : game_state -> unit

val display_random_ascii_art : string list -> unit

val move_monsters_and_check_game_over : game_state -> bool * string list

val update_camera_statuses : game_state -> unit

val print_map : unit -> unit

val random_hazard : game_state -> unit

val random_events : game_state -> unit

val resolve_hazard : game_state -> unit

val process_command : game_state -> string -> unit

val list_to_string : string list -> string

val game_loop : game_state -> unit

val choose_difficulty : unit -> difficulty

val start_or_tutorial : unit -> unit

val read_ascii_art_from_file : string -> string