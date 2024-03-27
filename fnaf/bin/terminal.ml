open Unix

[@@@ocaml.warning "-69"]
type game_state = {
  mutable battery: int;
  start_time: float;
  mutable door_closed: bool;
  mutable light_on: bool;
  mutable camera_statuses: (int * bool) list;
  mutable monster_locations: (string * int * float) list;
}
[@@@ocaml.warning "+69"]
let initial_state () = {
  battery = 100;
  start_time = gettimeofday ();
  door_closed = false;
  light_on = false;
  camera_statuses = List.init 5 (fun i -> (i + 1, false));
  monster_locations = [("Chica", 5, 0.); ("Foxy", 5, 0.); ("Bonnie", 5, 0.); ("Freddy Fazbear", 5, 0.)];
}

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
    let pace = match name with
      | "Chica" -> 20.0
      | "Foxy" -> 15.0
      | "Bonnie" -> 25.0
      | "Freddy Fazbear" -> 30.0
      | _ -> float_of_int max_int  
    in
    let should_move = (current_time -. last_move_time) >= pace in
    if should_move then
      let next_location = max 0 (location - 1) in  (* Calculate the next move *)
      if next_location = 0 then  (* Monster is about to enter the player's location *)
        if state.door_closed then
          (name, Random.int 6, current_time)  (* Reset to Camera 5 if the door is closed *)
        else
          begin
            game_over_info := (true, name);  (* Game over if the door isn't closed *)
            (name, 0, current_time)  (* Allow the monster to move to location 0 if the door isn't closed *)
          end
      else
        (name, next_location, current_time)  (* Normal movement for all other cases *)
    else
      (name, location, last_move_time)  (* No movement if it's not time to move yet *)
  ) state.monster_locations;
  !game_over_info

  

let update_camera_statuses state =
  state.camera_statuses <- List.init 5 (fun i ->
    let cam = i + 1 in
    (cam, List.exists (fun (_, loc, _) -> loc = cam) state.monster_locations))

let process_command state command =
  match command with
  | "door" ->
    state.door_closed <- not state.door_closed;
    state.battery <- state.battery - 5;
    print_endline (if state.door_closed then "Door closed." else "Door opened.")
  | "light" ->
    state.light_on <- not state.light_on;
    state.battery <- state.battery - 2;
    if state.light_on then
      let monsters_nearby = List.exists (fun (_, loc, _) -> loc = 1 || loc = 2) state.monster_locations in
      if monsters_nearby then print_endline "Monster nearby!" else print_endline "No monster nearby."
    else
      print_endline "Light turned off."
  | cam when String.starts_with cam ~prefix:"camera" ->
    (try
       let cam_number = String.sub cam 6 (String.length cam - 6) |> int_of_string in
       state.battery <- state.battery - 1;
       update_camera_statuses state;
       let monsters_in_cam = List.filter (fun (_, loc, _) -> loc = cam_number) state.monster_locations in
       if List.length monsters_in_cam > 0 then
         let monster_names = List.map (fun (name, _, _) -> name) monsters_in_cam in
         let spotted_msg = String.concat ", " monster_names in
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

let () =
  let state = initial_state () in
  print_endline "Welcome to Five Nights at Freddy's OCaml Edition.";
  print_map (); 
  game_loop state;;
