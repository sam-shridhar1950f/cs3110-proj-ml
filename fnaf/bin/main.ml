open Unix
open! Fnaf.Monster

[@@@ocaml.warning "-69"]

type power_mode =
  | Normal
  | PowerSaving

type game_state = {
  mutable battery : int;
  start_time : float;
  mutable door_closed : bool;
  mutable light_on : bool;
  mutable camera_statuses : (int * bool) list;
  mutable monsters : monster list;
  mutable power_mode : power_mode;
  mutable generator_on : bool; (* New field to track generator status *)
  difficulty : difficulty;
}

[@@@ocaml.warning "+69"]

let elapsed_time state = Unix.gettimeofday () -. state.start_time
let time_is_up state = elapsed_time state >= 516.0
let game_hour state = int_of_float (elapsed_time state /. 86.0)
(* 86 seconds = Hour *)

let initial_state difficulty =
  {
    battery = 100;
    start_time = gettimeofday ();
    door_closed = false;
    light_on = false;
    camera_statuses = List.init 5 (fun i -> (i + 1, false));
    monsters = init_monsters;
    power_mode = Normal;
    generator_on = false;
    difficulty;
  }

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

let process_command state command =
  match command with
  | "help" -> print_endline "List of Available Commands";
  print_endline "door - toggles the door open or closed.";
  print_endline "light - toggles the light on or off. Informs the user if a monster is visible nearby.";
  print_endline "toggle_generator - toggles the generator on add off. Increases the pace of monsters but recharges battery.";
  print_endline "toggle_power_mode - toggles the battery mode from normal to power-saving, and vice versa.";
  print_endline "camera - Usage: 'camera' followed by the desired camera number. Uses some battery and informs the user if a monster is in view.";
  print_endline "map - displays a map of the building, including camera numbers and locations."
  | "door" ->
      let power_cost = power_consumption_rates state "door" in
      state.door_closed <- not state.door_closed;
      state.battery <- state.battery - power_cost;
      print_endline
        (if state.door_closed then "Door closed." else "Door opened.")
  | "light" ->
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
  if time_is_up state then
    print_endline "Time's up. The night is over. You survived!"
  else if state.battery <= 0 then print_endline "Battery dead. You lost!"
  else begin
    Printf.printf "Hour: %d. Battery: %d%%. Type a command: " (game_hour state)
      state.battery;
    let command = read_line () in
    process_command state command;
    let game_over, monster_names = move_monsters_and_check_game_over state in
    if game_over then
      Printf.printf "A monster got you! It was %s! Game over.\n"
        (list_to_string monster_names)
    else game_loop state
  end


let choose_difficulty () : difficulty =
  print_endline "Choose difficulty: 1 - Easy, 2 - Normal, 3 - Hard";
  let choice = read_line () in
  match choice with
  | "1" -> Easy
  | "2" -> Normal
  | "3" -> Hard
  | _ ->
      print_endline "Invalid choice, defaulting to Normal.";
      Normal
  
let start_or_tutorial () = 
  print_endline "If you are familiar with FNAF gameplay, type start to begin. Otherwise, use the tutorial command to experiment with the game.";
  match read_line () with
  | "start" -> let difficulty = choose_difficulty () in
  let state = initial_state difficulty in
  print_map ();
  game_loop state
  | "tutorial" -> let state = initial_state Tutorial in
  print_map ();
  game_loop state
  | _ -> ()


let () =
  print_endline "Welcome to Five Nights at Freddy's OCaml Edition.";
  print_endline "Use the help command for a list of available commands.";
  start_or_tutorial ()
