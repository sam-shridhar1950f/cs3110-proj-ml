open! Unix
open! Fnaf.Monster
open! Fnaf.Game
open! ANSITerminal

let print_intro =
  print_string [ ANSITerminal.yellow ]
    "Welcome to Five Nights at Freddy's OCaml Edition.\n\n";
  print_string [ ANSITerminal.blue ]
    "Developed by Larry Tao, Sam Shridhar, Rohan Mahajan, Jacob Huang\n\n";
  print_string [ ANSITerminal.red ]
    "You’ve just started your shift as the new night security guard at Freddy \
     Fazbear's Pizza.\n\n";
  print_string [ ANSITerminal.red ]
    "Your job is to survive the night by monitoring the movements of the \
     animatronic monsters through the security cameras and preventing them \
     from reaching your location.\n\n";
  print_string [ ANSITerminal.red ] "Here’s the rundown:\n";
  print_string [ ANSITerminal.red ]
    "* Your Location: You are at the top of the map, behind a door.\n";
  print_string [ ANSITerminal.red ]
    "* Cameras: Use the cameras to track the monsters as they move from room \
     5, down through rooms 4, 3, 2, and 1, and finally towards your door.\n";
  print_string [ ANSITerminal.red ]
    "* Door Control: When the monsters get too close, use the door to block \
     them from reaching you. But be careful, as using the door consumes \
     power!\n\n";
  print_string [ ANSITerminal.red ]
    "Objective: Survive until morning by managing your power and resources \
     wisely. Keep a close eye on the cameras, and close the door only when \
     necessary to keep the monsters at bay.\n\n";
  print_string [ ANSITerminal.red ]
    "If you find you need help or don't know what to do, enter \"help\" as \
     your next command. Your shift starts now. Good luck, and stay safe!\n\n";
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
