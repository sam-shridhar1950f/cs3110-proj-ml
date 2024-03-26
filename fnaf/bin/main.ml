include Random
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
  start_time = Unix.gettimeofday ();
  door_closed = false;
  light_on = false;
  camera_statuses = List.init 5 (fun i -> (i + 1, false));
  monster_locations = [("Chica", 5, 0.); ("Foxy", 5, 0.); ("Bonnie", 5, 0.); ("Freddy Fazbear", 5, 0.)];
}


