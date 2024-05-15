open! Unix
open! Monster

type power_mode =
  | Typical
  | PowerSaving
(** type of power mode. *)

type hazard =
  | PowerSurge
  | LightMalfunction
  | DoorJam
(** type of game hazard.*)

type game_state
(** type of game state that tracks game variables.*)

val hourly_messages : string list
(** list of messages to be sent per hour. *)

val elapsed_time : game_state -> float
(** elapsed time since start of game *)

val time_is_up : game_state -> bool
(** whether or not time is up*)

val game_hour : game_state -> int
(** current hour of game *)

val gen_state : int -> float -> bool -> bool -> hazard option -> bool -> bool -> (int * bool) list -> monster list -> power_mode -> bool -> difficulty -> int -> float list -> game_state
(** generates a game state with given parameters *)

val get_battery : game_state -> int
(** returns current battery level*)

val get_start_time : game_state -> float
(** gets start time of game*)

val get_door_closed : game_state -> bool
(** returns whether the door is closed*)

val get_door_jammed : game_state -> bool
(** returns whether door is jammed *)

val get_hazard : game_state -> hazard option
(** returns current hazard or none otherwise*)

val get_light_on : game_state -> bool
(** returns whether the light is on*)

val get_light_malfunction : game_state -> bool
(** returns whether there is a light malfunction*)

val get_camera_statuses : game_state -> (int * bool) list
(** returns list of camera statuses (whether and which monster is in each camera)*)

val get_monsters : game_state -> monster list
(** returns list of monsters *)

val get_power_mode : game_state -> power_mode
(** gets current power mode*)

val get_generator_on : game_state -> bool
(** gets whether the generator is on*)

val get_difficulty : game_state -> difficulty
(** gets difficulty level of game*)

val get_last_announced_hour : game_state -> int
(** gets the most recently announced hour*)

val get_command_times : game_state -> float list
(** get list of times that correspond to each command*)

val initial_state : difficulty -> game_state
(** get initial state of the game based on difficulty*)

val list_to_string : string list -> string
(** [list_to_string ["a";"b";"c"]] is ["a, and b, and c"]*)

val update_command_times: game_state -> float -> unit
(** [update_command_times state time] adds [time] to [state]'s list of command times*)

val too_many_commands: game_state -> float -> bool
(** returns whether too many commands have been entered in a certain amount of time *)

val read_ascii_art_from_file : string -> string
(** converts a file to ascii art given the filename. *)

val power_consumption_rates : game_state -> string -> int
(** returns the corresponding power consumption rate of an action with the current power mode *)

val toggle_power_mode : game_state -> unit
(** flips the power mode from typical to power-saving and vice versa. *)

val operate_generator : game_state -> unit
(** turns the generator on or does nothing if it is already on. *)

val toggle_generator : game_state -> unit
(** toggles generator on or off.*)

val display_random_ascii_art : string list -> unit
(** given a list of filenames, displays a random one to the user.*)

val move_monsters_and_check_game_over : game_state -> bool -> bool * string list
(** [move_monsters_and_check_game_over state enraged] updates monster locations and checks whether the game has ended. *)

val update_camera_statuses : game_state -> unit
(** [update_camera_statuses state] updates the cameras on whether and which monsters are in each one. *)

val print_map : unit -> unit
(** prints an ascii art drawing of the game map with locations of the user, door, and cameras.*)

val apply_random_power_up : game_state -> unit
(** either gives +10 or -10 battery with equal chance of either one happening.*)

val random_hazard : game_state -> unit
(** Has a 5% chance of applying the 3 equally weighted hazards to the game.*)

val random_events : game_state -> unit
(** Has a 15% chance every hour of displaying a message about a random event. Does not affect game state.*)

val resolve_hazard : game_state -> unit
(** Stops current hazard or does nothing if no current hazard.*)

val process_command : game_state -> string -> unit
(** [process_command state command] does the preset action corresponding to [command] or prints an error message if the command
    is invalid.*)

val game_loop : game_state -> unit
(** [game_loop state] is recursive and loops until the game ends. With every iteration it updates the game state, triggers
    random events and hazards, and checks if the game ends. *)

val choose_difficulty : unit -> difficulty
(** converts user input from a number to corresponding difficulty (Easy, Normal, Hard). Requires a valid input. *)

val start_or_tutorial : unit -> unit
(** Starts the game with either an option to choose difficulty or a tutorial based on user input.*)