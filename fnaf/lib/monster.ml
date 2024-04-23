type monster = {
  name : string;
  mutable location : int;
  mutable last_move_time : float;
}

[@@@ocaml.warning "-37"]

type difficulty =
  | Easy
  | Normal
  | Hard

[@@@ocaml.warning "+37"]

let init_monsters =
  [
    { name = "Chica"; location = 5; last_move_time = 0. };
    { name = "Foxy"; location = 5; last_move_time = 0. };
    { name = "Bonnie"; location = 5; last_move_time = 0. };
    { name = "Freddy Fazbear"; location = 5; last_move_time = 0. };
  ]

let get_pace difficulty monster generator_on =
  let pace =
    match (difficulty, monster.name) with
    | Easy, "Chica" | Easy, "Foxy" | Easy, "Bonnie" | Easy, "Freddy Fazbear" ->
        30.0
    | Normal, "Chica" | Hard, "Foxy" -> 20.0
    | Normal, "Foxy" | Hard, "Chica" -> 15.0
    | Normal, "Bonnie" | Hard, "Bonnie" -> 25.0
    | Normal, "Freddy Fazbear" | Hard, "Freddy Fazbear" -> 30.0
    | _ -> float_of_int max_int
  in
  if generator_on && monster.location < 3 then pace -. 10. else pace

let move_monster monster current_time difficulty door_closed generator_on =
  Random.self_init ();
  let pace = get_pace difficulty monster generator_on in
  let should_move = current_time -. monster.last_move_time >= pace in
  if should_move then (
    let next_location = max 0 (monster.location - 1) in
    if next_location = 0 && door_closed = true then
      monster.location <- Random.int 4 + 1
    else monster.location <- next_location;
    monster.last_move_time <- current_time;
    if next_location = 0 && not door_closed then true else false)
  else false

let update_monsters monsters current_time difficulty door_closed gen_on =
  List.fold_left
    (fun (game_over, monsters_in_office) monster ->
      let reached_player =
        move_monster monster current_time difficulty door_closed gen_on
      in
      if reached_player then (true, monster.name :: monsters_in_office)
      else (game_over, monsters_in_office))
    (false, []) monsters

let get_locations monsters = List.map (fun m -> m.location) monsters

let get_monsters_at_location monsters loc =
  List.fold_right
    (fun monster acc ->
      if monster.location = loc then monster.name :: acc else acc)
    monsters []
