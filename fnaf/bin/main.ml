open! Unix
open! Fnaf.Monster
open! Fnaf.Game
open! ANSITerminal

let print_intro =
  print_string [ ANSITerminal.yellow ]
    "Welcome to Five Nights at Freddy's OCaml Edition.\n\n";
  print_string [ ANSITerminal.red ]
    "Use the help command for a list of available commands.\n\n";
  print_string [ ANSITerminal.blue ]
    "Developed by Larry Tao, Sam Shridhar, Rohan Mahajan, Jacob Huang\n\n";
  print_string [ ANSITerminal.blue ]
    "Tutorial Mode: Monsters do not move towards you. Feel free to explore the \
     commands.\n";
  print_string [ ANSITerminal.blue ]
    "Easy Mode: Monsters move slowly towards you.\n";
  print_string [ ANSITerminal.blue ]
    "Normal Mode: Monsters move at a normal pace towards you.\n";
  print_string [ ANSITerminal.blue ]
    "Hard Mode: Monsters move at a fast pace towards you.\n"

let () =
  print_intro;
  let filename = "data/logo.txt" in
  try
    let ascii_art = read_ascii_art_from_file filename in
    print_endline ascii_art;
    (* Ensure this line ends with a semicolon *)
    start_or_tutorial () (* Proceed with the next function call *)
  with
  | Sys_error msg -> print_endline ("File error: " ^ msg)
  (* End with a semicolon if more code follows *)
  | e ->
      print_endline ("An unexpected error occurred: " ^ Printexc.to_string e);
      (* Ensure this line ends with a semicolon *)
      start_or_tutorial ()
