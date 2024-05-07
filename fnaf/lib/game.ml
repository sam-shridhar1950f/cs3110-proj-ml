open! Unix
open! Monster

type power_mode =
  | Normal
  | PowerSaving

type hazard =
  | PowerSurge
  | LightMalfunction
  | DoorJam

[@@@ocaml.warning "-69"]

type game_state = {
  mutable battery : int;
  start_time : float;
  mutable door_closed : bool;
  mutable door_jammed : bool;
  mutable hazard : hazard option;
      (* Optional hazard currently affecting the game *)
  mutable light_on : bool;
  mutable light_malfunction : bool;
  mutable camera_statuses : (int * bool) list;
  mutable monsters : monster list;
  mutable power_mode : power_mode;
  mutable generator_on : bool;
  difficulty : difficulty;
  mutable last_announced_hour : int;
      (* New field to track the last announced hour *)
}

[@@@ocaml.warning "+69"]

let hourly_messages =
  [
    "The clock strikes midnight. The faint hum of electronics is all that can \
     be heard.";
    "It's 1 AM. Somewhere in the darkness, gears turn and mechanisms whir.";
    "2 AM. You hear a distant shuffling. Something is moving.";
    "3 AM. The walls seem to close in around you. Keep watching those cameras.";
    "4 AM. A chill runs down your spine. The night is almost over, but danger \
     increases.";
    "5 AM. The dawn is near. Just a little longer, you can make it!";
  ]

let elapsed_time state = Unix.gettimeofday () -. state.start_time
let time_is_up state = elapsed_time state >= 516.0
let game_hour state = int_of_float (elapsed_time state /. 86.0)
(* 86 seconds = Hour *)

let initial_state difficulty =
  {
    battery = 100;
    start_time = Unix.gettimeofday ();
    door_closed = false;
    door_jammed = false;
    light_on = false;
    light_malfunction = false;
    camera_statuses = List.init 5 (fun i -> (i + 1, false));
    monsters = init_monsters;
    power_mode = Normal;
    generator_on = false;
    hazard = None;
    difficulty;
    last_announced_hour = -1;
    (* Initialize to -1 so it will definitely update on the first hour *)
  }

let read_ascii_art_from_file filename =
  let input_channel = open_in filename in
  let rec read_lines accum =
    try
      let line = input_line input_channel in
      read_lines (accum ^ line ^ "\n")
    with End_of_file -> accum
  in
  try
    let art = read_lines "" in
    close_in input_channel;
    art
  with e ->
    close_in_noerr input_channel;
    (* Ensure the file is closed even if an error occurs *)
    raise e (* Re-raise the exception after handling it *)

let display_ascii_art filename =
  let full_path = "data/" ^ filename ^ ".txt" in
  try
    let ascii_art = read_ascii_art_from_file full_path in
    print_endline ascii_art
  with
  | Sys_error msg -> print_endline ("Error loading file: " ^ msg)
  | _ -> print_endline "Failed to display ASCII art."

let power_consumption_rates state action : int =
  match (state.power_mode, action) with
  | Normal, "door" -> 5
  | PowerSaving, "door" -> 3
  | Normal, "light" -> 2
  | PowerSaving, "light" -> 1
  | Normal, "camera" -> 1
  | PowerSaving, "camera" -> 1
  | _ -> 0

let toggle_power_mode state =
  state.power_mode <-
    (if state.power_mode = Normal then PowerSaving else Normal);
  print_endline
    (if state.power_mode = Normal then "Normal power mode activated."
     else "Power-saving mode activated.")

let operate_generator state =
  if state.battery < 100 then begin
    state.battery <- min 100 (state.battery + 10);
    print_endline "Generator on, battery charging..."
  end
  else print_endline "Battery is already full."

let toggle_generator state =
  let _ =
    update_monsters state.monsters (elapsed_time state) state.difficulty
      state.door_closed state.generator_on
  in
  if state.generator_on then begin
    state.generator_on <- false;
    print_endline "Generator turned off."
  end
  else if state.battery < 100 then begin
    state.generator_on <- true;
    state.battery <- min 100 (state.battery + 10);
    print_endline "Generator turned on, battery charging..."
  end
  else print_endline "Battery full. Generator not needed."

let display_random_ascii_art filenames =
  Random.self_init ();
  let index = Random.int (List.length filenames) in
  let filename = List.nth filenames index in
  let display_file filename =
    let channel = open_in filename in
    try
      while true do
        let line = input_line channel in
        print_endline line
      done
    with
    | End_of_file -> close_in channel
    | e ->
        close_in_noerr channel;
        raise e
  in
  display_file filename

let move_monsters_and_check_game_over state =
  let current_time = elapsed_time state in
  update_monsters state.monsters current_time state.difficulty state.door_closed
    state.generator_on

let update_camera_statuses state =
  state.camera_statuses <-
    List.init 5 (fun i ->
        let cam = i + 1 in
        (cam, List.exists (fun loc -> loc = cam) (get_locations state.monsters)))

let print_map () =
  let map_lines =
    [|
      "Map Layout:";
      "      [User]";
      "        ||";
      "        ||";
      "      [Door]";
      "        ||";
      "        ||";
      "  [Camera 1] ------ [Camera 2]";
      "      ||               ||";
      "      ||               ||";
      "[Camera 3]         [Camera 4]";
      "      \\               //";
      "       \\             //";
      "        \\           //";
      "         [Camera 5]";
    |]
  in
  Array.iter print_endline map_lines;
  print_newline ()

let apply_random_power_up state =
  Random.self_init ();
  if Random.bool () then begin
    (* 50% chance to get a beneficial power-up *)
    state.battery <- min 100 (state.battery + 10);
    (* Ensure battery doesn't exceed 100 *)
    print_endline "Mystery power-up activated: +10 battery power!"
  end
  else begin
    (* 50% chance to get a detrimental effect *)
    state.battery <- max 0 (state.battery - 10);
    (* Ensure battery doesn't go below 0 *)
    print_endline "Mystery debuff activated: -10 battery power!"
  end

let random_hazard state =
  if Random.float 1.0 < 0.05 then
    (* 5% chance to trigger a hazard each hour *)
    match Random.int 3 with
    | 0 ->
        state.hazard <- Some PowerSurge;
        state.camera_statuses <-
          List.map (fun (id, _) -> (id, false)) state.camera_statuses;
        print_endline "A power surge has disabled all cameras!"
    | 1 ->
        state.hazard <- Some LightMalfunction;
        state.light_malfunction <- true;
        print_endline "There is a malfunction in the lighting system!"
    | 2 ->
        state.hazard <- Some DoorJam;
        state.door_jammed <- true;
        print_endline "The door mechanism is jammed!"
    | _ -> ()

let random_events state =
  if Random.float 1.0 < 0.15 then
    (* 15% chance for a random event each hour *)
    match Random.int 3 with
    | 0 ->
        print_endline
          "You notice an old newspaper article about missing persons. Ignore \
           it and focus!"
    | 1 ->
        print_endline
          "The air grows cold. Your camera flickers briefly. Something feels \
           very wrong."
    | 2 ->
        state.door_jammed <- true;
        print_endline "You hear a loud bang."
    | _ -> ()

let resolve_hazard state =
  match state.hazard with
  | Some PowerSurge ->
      state.camera_statuses <-
        List.map (fun (id, _) -> (id, true)) state.camera_statuses
  | Some LightMalfunction -> state.light_malfunction <- false
  | Some DoorJam -> state.door_jammed <- false
  | None -> ()

let process_command state command =
  match command with
  | "help" ->
      print_endline "List of Available Commands";
      print_endline "door - toggles the door open or closed.";
      print_endline
        "light - toggles the light on or off. Informs the user if a monster is \
         visible nearby.";
      print_endline
        "toggle_generator - toggles the generator on add off. Increases the \
         pace of monsters but recharges battery.";
      print_endline
        "toggle_power_mode - toggles the battery mode from normal to \
         power-saving, and vice versa.";
      print_endline
        "camera - Usage: 'camera' followed by the desired camera number. Uses \
         some battery and informs the user if a monster is in view.";
      print_endline
        "map - displays a map of the building, including camera numbers and \
         locations."
  | "door" ->
      if state.door_jammed then
        print_endline "The door is jammed and will not respond!"
      else
        let power_cost = power_consumption_rates state "door" in
        state.door_closed <- not state.door_closed;
        state.battery <- state.battery - power_cost;
        print_endline
          (if state.door_closed then "Door closed." else "Door opened.")
  | "light" ->
      if state.light_malfunction then
        print_endline "The lights are malfunctioning and will not respond!"
      else
        let power_cost = power_consumption_rates state "light" in
        state.light_on <- not state.light_on;
        state.battery <- state.battery - power_cost;
        if state.light_on then
          let monsters_nearby =
            List.exists (fun x -> x = 1 || x = 2) (get_locations state.monsters)
          in
          if monsters_nearby then print_endline "Monster nearby!"
          else print_endline "No monster nearby."
        else print_endline "Light turned off."
  | "generator" -> operate_generator state
  | "toggle_generator" -> toggle_generator state
  | "toggle_power_mode" -> toggle_power_mode state
  | cam when String.starts_with cam ~prefix:"camera" -> (
      try
        let cam_number =
          String.sub cam 6 (String.length cam - 6) |> int_of_string
        in
        let power_cost = power_consumption_rates state "camera" in
        state.battery <- state.battery - power_cost;
        update_camera_statuses state;
        let monster_names =
          get_monsters_at_location state.monsters cam_number
        in
        if List.length monster_names > 0 then (
          let spotted_msg = String.concat ", " monster_names in
          let filepaths =
            List.map (fun name -> "data/" ^ name ^ ".txt") monster_names
          in
          display_random_ascii_art filepaths;
          Printf.printf "Monsters spotted at Camera %d: %s\n" cam_number
            spotted_msg)
        else Printf.printf "Camera %d: Clear.\n" cam_number
      with
      | Failure _ -> print_endline "Invalid camera number."
      | Not_found -> print_endline "Camera not found.")
  | "map" -> print_map ()
  | name
    when List.exists (fun monster -> get_name monster = name) state.monsters ->
      display_ascii_art name
  | _ -> print_endline "Invalid command"

let list_to_string lst =
  let rec aux acc = function
    | [] -> acc
    | [ t ] -> acc ^ ", and " ^ t
    | h :: t -> aux (acc ^ h ^ ", ") t
  in
  match lst with
  | [] -> ""
  | [ t ] -> t
  | items -> aux "" items

let rec game_loop state =
  let current_hour = game_hour state in
  if time_is_up state then
    print_endline "Time's up. The night is over. You survived!"
  else if state.battery <= 0 then print_endline "Battery dead. You lost!"
  else begin
    if current_hour <> state.last_announced_hour then begin
      if current_hour < List.length hourly_messages then
        print_endline (List.nth hourly_messages current_hour);
      apply_random_power_up state;
      (* Apply a power-up or debuff each hour *)
      state.last_announced_hour <- current_hour
      (* Update the last announced hour *)
    end;
    (* Note the semicolon here *)
    random_hazard state;
    (* Check for new hazards *)
    random_events state;
    (* Trigger potential random events *)
    Printf.printf "Hour: %d. Battery: %d%%. Type a command: " (game_hour state)
      state.battery;
    let command = read_line () in
    process_command state command;
    resolve_hazard state;
    (* Resolve any existing hazards *)
    let game_over, monster_names = move_monsters_and_check_game_over state in
    if game_over then
      Printf.printf "A monster got you! It was %s! Game over.\n"
        (list_to_string monster_names)
    else game_loop state
  end

let choose_difficulty () : difficulty =
  print_endline "\nChoose difficulty: 1 - Easy, 2 - Normal, 3 - Hard";
  let choice = read_line () in
  match choice with
  | "1" -> Easy
  | "2" -> Normal
  | "3" -> Hard
  | _ ->
      print_endline "Invalid choice, defaulting to Normal.";
      Normal

let rec start_or_tutorial () =
  print_endline
    "If you are familiar with FNAF gameplay, type \"start\" to begin. Use the \
     \"tutorial\" command to experiment with the game.";
  match read_line () with
  | "start" ->
      let difficulty = choose_difficulty () in
      let state = initial_state difficulty in
      print_map ();
      game_loop state
  | "tutorial" ->
      let state = initial_state Tutorial in
      print_map ();
      game_loop state
  | _ -> start_or_tutorial ()
