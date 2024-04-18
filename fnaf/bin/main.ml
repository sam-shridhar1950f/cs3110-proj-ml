open Unix

[@@@ocaml.warning "-69"]
type difficulty = Easy | Normal | Hard
type power_mode = Normal | PowerSaving
type game_state = {
  mutable battery: int;
  start_time: float;
  mutable door_closed: bool;
  mutable light_on: bool;
  mutable camera_statuses: (int * bool) list;
  mutable monster_locations: (string * int * float) list;
  mutable power_mode: power_mode;
  mutable generator_on: bool;  (* New field to track generator status *)
  difficulty: difficulty;
}

[@@@ocaml.warning "+69"]

let get_pace difficulty name =
  match (difficulty, name) with
  | (Easy, "Chica") | (Easy, "Foxy") | (Easy, "Bonnie") | (Easy, "Freddy Fazbear") -> 30.0
  | (Normal, "Chica") | (Hard, "Foxy") -> 20.0
  | (Normal, "Foxy") | (Hard, "Chica") -> 15.0
  | (Normal, "Bonnie") | (Hard, "Bonnie") -> 25.0
  | (Normal, "Freddy Fazbear") | (Hard, "Freddy Fazbear") -> 30.0
  | _ -> float_of_int max_int

  let initial_state difficulty = {
    battery = 100;
    start_time = gettimeofday ();
    door_closed = false;
    light_on = false;
    camera_statuses = List.init 5 (fun i -> (i + 1, false));
    monster_locations = [("Chica", 5, 0.); ("Foxy", 5, 0.); ("Bonnie", 5, 0.); ("Freddy Fazbear", 5, 0.)];
    power_mode = Normal;
    generator_on = false;  (* Initialize the generator status as off *)
    difficulty;
  }
  

let power_consumption_rates state action =
  match (state.power_mode, action) with
  | (Normal, "door") -> 5
  | (PowerSaving, "door") -> 3
  | (Normal, "light") -> 2
  | (PowerSaving, "light") -> 1
  | (Normal, "camera") -> 1
  | (PowerSaving, "camera") -> 1
  | _ -> 0

let toggle_power_mode state =
  state.power_mode <- if state.power_mode = Normal then PowerSaving else Normal;
  print_endline (if state.power_mode = Normal then "Normal power mode activated." else "Power-saving mode activated.")

let operate_generator state =
  if state.battery < 100 then
    begin
      state.battery <- min 100 (state.battery + 10);  (* Recharge battery *)
      print_endline "Generator on, battery charging...";
      (* Adjust the last move time for monsters to simulate faster movement due to noise *)
      state.monster_locations <- List.map (fun (name, loc, last_move_time) ->
        if loc < 3 then (name, loc, last_move_time -. 10.0)
        else (name, loc, last_move_time)
      ) state.monster_locations;
    end
  else
    print_endline "Battery is already full."

let toggle_generator state =
  if state.generator_on then
    begin
      state.generator_on <- false;
      print_endline "Generator turned off.";
    end
  else if state.battery < 100 then
    begin
      state.generator_on <- true;
      state.battery <- min 100 (state.battery + 10);  (* Assume immediate charging for simplicity *)
      print_endline "Generator turned on, battery charging...";
      (* Simulating noise causing monsters to move potentially faster *)
      state.monster_locations <- List.map (fun (name, loc, last_move_time) ->
        if loc < 3 then (name, loc, last_move_time -. 10.0)
        else (name, loc, last_move_time)
      ) state.monster_locations;
    end
  else
    print_endline "Battery full. Generator not needed."



let display_random_ascii_art filenames =
  Random.self_init ();  (* Initialize the random number generator *)

  (* Choose a random index in the list of filenames *)
  let index = Random.int (List.length filenames) in
  let filename = List.nth filenames index in

  (* Function to read and print the file *)
  let display_file filename =
    let channel = open_in filename in
    try
      while true do
        let line = input_line channel in
        print_endline line
      done
    with
    | End_of_file -> close_in channel
    | e -> close_in_noerr channel; raise e
  in

  (* Display the chosen file *)
  display_file filename

let elapsed_time state =
  Unix.gettimeofday () -. state.start_time

let time_is_up state =
  elapsed_time state >= 516.0

let game_hour state =
  int_of_float (elapsed_time state /. 86.0) (* 86 seconds = Hour *)

let move_monsters_and_check_game_over state =
  let current_time = elapsed_time state in
  let game_over_info = ref (false, "") in
  state.monster_locations <- List.map (fun (name, location, last_move_time) ->
    let pace = get_pace state.difficulty name in
    let should_move = (current_time -. last_move_time) >= pace in
    if should_move then
      let next_location = max 0 (location - 1) in
      if next_location = 0 then
        if state.door_closed then
          (name, Random.int 6, current_time)
        else
          begin
            game_over_info := (true, name);
            (name, 0, current_time)
          end
      else
        (name, next_location, current_time)
    else
      (name, location, last_move_time)
  ) state.monster_locations;
  !game_over_info

let update_camera_statuses state =
  state.camera_statuses <- List.init 5 (fun i ->
    let cam = i + 1 in
    (cam, List.exists (fun (_, loc, _) -> loc = cam) state.monster_locations))

let process_command state command =
  match command with
  | "door" ->
    let power_cost = power_consumption_rates state "door" in
    state.door_closed <- not state.door_closed;
    state.battery <- state.battery - power_cost;
    print_endline (if state.door_closed then "Door closed." else "Door opened.")
  | "light" ->
    let power_cost = power_consumption_rates state "light" in
    state.light_on <- not state.light_on;
    state.battery <- state.battery - power_cost;
    if state.light_on then
      let monsters_nearby = List.exists (fun (_, loc, _) -> loc = 1 || loc = 2) state.monster_locations in
      if monsters_nearby then print_endline "Monster nearby!" else print_endline "No monster nearby."
    else
      print_endline "Light turned off."
  | "generator" -> operate_generator state
  | "toggle_generator" -> toggle_generator state
  | "toggle_power_mode" -> toggle_power_mode state
  | cam when String.starts_with cam ~prefix:"camera" ->
    (try
       let cam_number = String.sub cam 6 (String.length cam - 6) |> int_of_string in
       let power_cost = power_consumption_rates state "camera" in
       state.battery <- state.battery - power_cost;
       update_camera_statuses state;
       let monsters_in_cam = List.filter (fun (_, loc, _) -> loc = cam_number) state.monster_locations in
       if List.length monsters_in_cam > 0 then
         let monster_names = List.map (fun (name, _, _) -> name) monsters_in_cam in
         let spotted_msg = String.concat ", " monster_names in
         let filepaths = List.map (fun name -> "data/" ^ name ^ ".txt") monster_names in
         display_random_ascii_art filepaths;
         Printf.printf "Monsters spotted at Camera %d: %s\n" cam_number spotted_msg
       else
        Printf.printf "Camera %d: Clear.\n" cam_number
     with
     | Failure _ -> print_endline "Invalid camera number."
     | Not_found -> print_endline "Camera not found.")
  | _ -> print_endline "Invalid command"


let rec game_loop state =
  if time_is_up state then
    print_endline "Time's up. The night is over. You survived!"
  else if state.battery <= 0 then
    print_endline "Battery dead. You lost!"
  else begin
    Printf.printf "Hour: %d. Battery: %d%%. Type a command: " (game_hour state) state.battery;
    let command = read_line () in
    process_command state command;
    let (game_over, monster_name) = move_monsters_and_check_game_over state in
    if game_over then
      Printf.printf "A monster got you! It was %s! Game over.\n" monster_name
    else
      game_loop state
  end

let print_map () =
  let map_lines = [|
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
    "         [Camera 5]"
  |] in
  Array.iter print_endline map_lines;
  print_newline ();;

let choose_difficulty () =
  print_endline "Choose difficulty: 1 - Easy, 2 - Normal, 3 - Hard";
  let choice = read_line () in
  match choice with
  | "1" -> Easy
  | "2" -> Normal
  | "3" -> Hard
  | _ -> print_endline "Invalid choice, defaulting to Normal."; Normal

let () =
  let difficulty = choose_difficulty () in
  let state = initial_state difficulty in
  print_endline "Welcome to Five Nights at Freddy's OCaml Edition.";
  print_map ();
  game_loop state;;
