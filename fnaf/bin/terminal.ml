open Graphics
open Unix

[@@@ocaml.warning "-69"]

type game_state = {
  mutable battery : int;
  start_time : float;
  mutable door_closed : bool;
  mutable light_on : bool;
  mutable camera_statuses : (int * bool) list;
  mutable monster_locations : (string * int * float) list;
}

[@@@ocaml.warning "+69"]

let initial_state () =
  {
    battery = 100;
    start_time = gettimeofday ();
    door_closed = false;
    light_on = false;
    camera_statuses = List.init 5 (fun i -> (i + 1, false));
    monster_locations =
      [
        ("Chica", 5, 0.);
        ("Foxy", 5, 0.);
        ("Bonnie", 5, 0.);
        ("Freddy Fazbear", 5, 0.);
      ];
  }

let clear_window color =
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

let new_game () =
  clear_window black;
  set_color white;
  moveto 500 500;
  draw_string "Welcome to Five Days In Office Hours.";
  moveto 500 300;
  draw_string "Start Game: s";
  let e = wait_next_event [ Key_pressed ] in
  let user_command =
    match e.key with
    | ';' -> close_graph ()
    | _ -> ()
  in
  user_command

let load_game () =
  clear_window black;
  set_color white;
  moveto 500 500;
  draw_string "Select a game to load.";
  let e = wait_next_event [ Key_pressed ] in
  let user_command =
    match e.key with
    | ';' -> close_graph ()
    | _ -> ()
  in
  user_command

let settings () =
  clear_window black;
  set_color white;
  moveto 500 500;
  draw_string "Game Settings";
  let e = wait_next_event [ Key_pressed ] in
  let user_command =
    match e.key with
    | ';' -> close_graph ()
    | _ -> ()
  in
  user_command

let start_screen () =
  clear_window black;
  set_color white;
  moveto 200 600;
  draw_string "New Game: n";
  moveto 200 500;
  draw_string "Load Game: l";
  moveto 200 400;
  draw_string "Settings: s";
  let e = wait_next_event [ Key_pressed ] in
  let user_command =
    match e.key with
    | ';' -> close_graph ()
    | 'n' -> new_game ()
    | 'l' -> load_game ()
    | 's' -> settings ()
    | _ -> ()
  in
  user_command

let loading_screen () =
  open_graph " 1280x780";
  open_graph "";
  clear_window black;
  set_color yellow;
  moveto 500 150;
  draw_string "Press [ s ] to start... ";
  moveto 650 20;
  set_color white;
  draw_string "Developed by Larry Tao, Sam Shridhar, Rohan Mahajan, Jacob Huang";
  synchronize ();
  let e = wait_next_event [ Key_pressed ] in
  if e.key = ';' then close_graph () else if e.key = 's' then start_screen ()

let main_menu () =
  loading_screen ();
  synchronize ();
  close_graph ()

let _ = initial_state ()
let () = main_menu ()