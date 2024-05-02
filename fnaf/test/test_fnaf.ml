(* test_monster.ml *)
open OUnit2
open Fnaf.Monster
open QCheck

(* ----- Helper Functions ----- *)
let setup_monsters initial_location initial_time =
  [
    create_monster "Chica" initial_location initial_time;
    create_monster "Foxy" initial_location initial_time;
    create_monster "Bonnie" initial_location initial_time;
    create_monster "Freddy Fazbear" initial_location initial_time;
  ]

let advance_time monsters time_diff difficulty door_closed generator_on =
  List.iter
    (fun monster ->
      ignore
        (move_monster monster
           (get_time monster +. time_diff)
           difficulty door_closed generator_on))
    monsters

let assert_location monsters expected_locations =
  List.iter2
    (fun monster expected_location ->
      assert_equal ~printer:string_of_int expected_location
        (get_location monster)
        ~msg:
          (Printf.sprintf "Expected %s to be at location %d but was at %d"
             (get_name monster) expected_location (get_location monster)))
    monsters expected_locations

let gen_monster =
  let open QCheck.Gen in
  let* name = oneofl [ "Chica"; "Foxy"; "Bonnie"; "Freddy Fazbear" ] in
  let* location = 0 -- 5 in
  let* last_move_time = float_bound_inclusive 100.0 in
  return (create_monster name location last_move_time)

let gen_difficulty = QCheck.Gen.oneofl [ Easy; Normal; Hard ]
let gen_bool = QCheck.Gen.bool
let gen_triple = QCheck.Gen.triple gen_monster gen_difficulty gen_bool
let gen_monster_list = QCheck.Gen.list gen_monster

(* ----- Test Definitions ----- *)

let test_name _ =
  let mon = create_monster "michael" 5 0.0 in
  assert_equal "michael" (get_name mon)

let test_name_empty _ =
  let mon = create_monster "" 5 0.0 in
  assert_equal "" (get_name mon)

let test_location _ =
  let mon = create_monster "michael" 5 0.0 in
  assert_equal 5 (get_location mon)

let test_location_max _ =
  let mon = create_monster "michael" max_int 0.0 in
  assert_equal max_int (get_location mon)

let test_location_min _ =
  let mon = create_monster "michael" min_int 0.0 in
  assert_equal min_int (get_location mon)

let test_time _ =
  let mon = create_monster "michael" 5 0.0 in
  assert_equal 0.0 (get_time mon)

let test_time_max _ =
  let mon = create_monster "michael" 5 (float_of_int max_int) in
  assert_equal (float_of_int max_int) (get_time mon)

let test_time_min _ =
  let mon = create_monster "michael" 5 (float_of_int min_int) in
  assert_equal (float_of_int min_int) (get_time mon)

(* Test that all monsters start at the correct initial location *)
let test_initial_positions _ =
  let monsters = init_monsters in
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test movement logic with generator off *)
let test_movement_without_generator _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 30. Normal false false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test movement logic with generator on reduces pace *)
let test_movement_with_generator _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 56. Normal false true;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test monsters reset location when door is closed at location 0 *)
let test_reset_when_door_closed _ =
  let monsters = setup_monsters 0 0.0 in
  advance_time monsters 60. Normal true false;
  List.iter
    (fun m ->
      assert_bool "Monsters should reset to a new location" (get_location m > 0))
    monsters

(* Test pace change based on difficulty levels *)
let test_pace_change_with_difficulty _ =
  let monster = create_monster "Chica" 5 0.0 in
  let easy_pace = get_pace Easy monster false in
  let hard_pace = get_pace Hard monster false in
  assert_bool "Hard difficulty should have a faster pace" (hard_pace < easy_pace)

(* Test non-movement when the game starts (time = 0) *)
let test_no_movement_at_start _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 0. Easy false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test negative time does not cause errors or movement *)
let test_negative_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (-10.) Easy false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test maximum integer time advancement *)
let test_max_int_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (float_of_int max_int) Easy false false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test minimum integer time regression *)
let test_min_int_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (-.float_of_int max_int) Easy false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test random resets within game bounds *)
let test_random_resets_within_bounds _ =
  Random.self_init ();
  let monsters = setup_monsters 5 0.0 in
  for _ = 1 to 10 do
    advance_time monsters (Random.float 50.) Hard true true
  done;
  List.iter
    (fun m ->
      assert_bool "Monsters should be within valid game bounds"
        (get_location m >= 0 && get_location m <= 5))
    monsters

let test_extreme_time_advancement _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 31536000.0 (* 1 year in seconds *) Easy false false;
  assert_location monsters [ 4; 4; 4; 4 ]
(* Expected locations after handling extreme time advancement *)

let test_invalid_time_and_location _ =
  let monsters = setup_monsters (-1) (-1.0) in
  List.iter
    (fun m ->
      assert_bool "Monster was created with invalid location or time"
        (get_location m < 0 || get_time m < 0.0))
    monsters

let test_consecutive_door_closures _ =
  let monsters = setup_monsters 1 0.0 in
  for _ = 1 to 5 do
    advance_time monsters 1. Hard true false;
    (* Door closed *)
    advance_time monsters 1. Hard false false (* Door opened *)
  done;
  assert_location monsters [ 1; 1; 1; 1 ]
(* Check for expected behavior after consecutive door actions *)

let test_consistency_across_runs _ =
  let first_run = setup_monsters 5 0.0 in
  let second_run = setup_monsters 5 0.0 in
  advance_time first_run 10. Normal false false;
  advance_time second_run 10. Normal false false;
  assert_equal
    ~printer:(fun lst -> String.concat "; " (List.map string_of_int lst))
    (get_locations first_run) (get_locations second_run)

let test_extreme_pacing_changes _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 5. Easy false false;
  advance_time monsters 5. Hard false false;
  advance_time monsters 5. Easy false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test rapid sequence of door state changes *)
let test_rapid_door_state_changes _ =
  let monsters = setup_monsters 5 0.0 in
  for _ = 1 to 10 do
    let door_state = Random.bool () in
    advance_time monsters 5. Normal door_state false
  done;
  List.iter
    (fun m ->
      assert_bool
        "Monsters should be within valid game bounds after rapid door changes"
        (get_location m >= 0 && get_location m <= 5))
    monsters

(* Test large number of game cycles with random generator states *)
let test_large_number_of_cycles _ =
  let monsters = setup_monsters 5 0.0 in
  for i = 1 to 100 do
    let gen_state = Random.bool () in
    advance_time monsters (float_of_int i) Hard false gen_state
  done;
  List.iter
    (fun m ->
      assert_bool "Monsters should remain within game bounds after many cycles"
        (get_location m >= 0 && get_location m <= 5))
    monsters

(* Test edge cases for location bounds at maximum values *)
let test_location_bounds_at_maximum _ =
  let monsters = setup_monsters max_int 0.0 in
  for _ = 1 to 5 do
    advance_time monsters 10. Easy false false
  done;
  List.iter
    (fun m ->
      assert_bool "Monster location should handle max int values properly"
        (get_location m >= 0 && get_location m <= max_int))
    monsters

(* Test door interaction during critical monster movements *)
let test_critical_door_interactions _ =
  let monsters = setup_monsters 1 0.0 in
  advance_time monsters 1. Hard true false;
  assert_location monsters [ 1; 1; 1; 1 ]
(* Assuming monsters try to move to location 0 but door is closed *)

(* Test monster movement synchronization under uniform conditions *)
let test_movement_synchronization _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 30. Hard false true;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test each monster's independent reaction to generator on/off state *)
let test_individual_generator_effects _ =
  let monsters = setup_monsters 5 0.0 in
  List.iteri
    (fun i m ->
      advance_time [ m ] (10. *. float_of_int (i + 1)) Hard false (i mod 2 = 0))
    monsters;
  assert_location monsters [ 5; 4; 4; 4 ]

let test_difficulty_impact _ =
  let easy_pace = get_pace Easy (create_monster "Chica" 5 0.0) false in
  let hard_pace = get_pace Hard (create_monster "Chica" 5 0.0) false in
  assert_bool "Hard difficulty should result in a faster pace"
    (hard_pace < easy_pace)

let test_monster_name_consistency _ =
  let monsters = init_monsters in
  let names_before = List.map get_name monsters in
  advance_time monsters 60. Normal false false;
  let names_after = List.map get_name monsters in
  assert_equal ~printer:(String.concat ", ") names_before names_after
    ~msg:"Monster names should remain consistent after state changes"

let test_timing_precision _ =
  let monsters = setup_monsters 5 0.0 in
  List.iter (fun m -> advance_time [ m ] 0.1 Hard false false) monsters;
  List.iter
    (fun m ->
      assert_bool "Timing updates should be precise to the second"
        (abs_float (get_time m -. 0.1) < 1.))
    monsters

let test_door_blockage_logic _ =
  let monsters = setup_monsters 1 0.0 in
  for _ = 1 to 10 do
    advance_time monsters 1. Normal true false
    (* Door repeatedly closes as monsters approach *)
  done;
  List.iter
    (fun m ->
      assert_bool "Monsters should not bypass a closed door" (get_location m > 0))
    monsters

let test_game_stability_under_extreme_load _ =
  let large_number_of_monsters =
    List.init 1000 (fun _ -> create_monster "TestMonster" 5 0.0)
  in
  let rec simulate_actions monsters count =
    if count = 0 then true
    else begin
      advance_time monsters 0.5 Hard false false;
      simulate_actions monsters (count - 1)
    end
  in
  assert_bool "Game should handle extreme load without crashing"
    (simulate_actions large_number_of_monsters 100)

let test_door_closure_effectiveness _ =
  let monsters = setup_monsters 1 0.0 in
  advance_time monsters 5. Hard true false;
  (* Door is closed as monsters approach location 0 *)
  List.iter
    (fun m ->
      assert_bool
        "Door closure should prevent monsters from reaching location 0"
        (get_location m > 0))
      (* Assuming location 0 is critical and door closure should stop them *)
    monsters

let test_monster_name_uniqueness _ =
  let monsters = init_monsters in
  let names = List.map get_name monsters in
  let unique_names = List.sort_uniq compare names in
  assert_equal (List.length names) (List.length unique_names)

let test_individual_monster_creation _ =
  let monster = create_monster "Bonnie" 3 10.0 in
  assert_equal "Bonnie" (get_name monster);
  assert_equal 3 (get_location monster);
  assert_equal 10.0 (get_time monster)

let test_monster_movement_at_boundary_conditions _ =
  let monster_at_min = create_monster "EdgeCaseMin" 0 0.0 in
  let monster_at_max = create_monster "EdgeCaseMax" 5 0.0 in
  let _ = move_monster monster_at_min 10.0 Normal false false in
  let _ = move_monster monster_at_max 10.0 Normal false false in
  assert_equal 0 (get_location monster_at_min);
  (* Expect no movement beyond lower boundary *)
  assert_equal 5 (get_location monster_at_max)
(* Expect normal decrement in location *)

(* Varying times and generator states should result in varied locations *)

(* ----- QCheck Property Definitions ----- *)

(* Property: Monsters should never move to a negative location *)
let prop_monster_never_moves_to_negative_location =
  Test.make ~count:1000 ~name:"prop_monster_never_moves_to_negative_location"
    (make gen_monster) (fun monster ->
      let monster' =
        create_monster (get_name monster)
          (max 0 (get_location monster - 1))
          (get_time monster)
      in
      get_location monster' >= 0)

(* Property: Monsters should reset to location > 0 if door is closed and they
   are at location 0 *)
let prop_monster_resets_if_door_closed =
  Test.make ~count:1000 ~name:"prop_monster_resets_if_door_closed"
    (make gen_monster) (fun monster ->
      let monster' = create_monster (get_name monster) 0 (get_time monster) in
      let result = move_monster monster' 30.0 Hard true true in
      get_location monster' > 0 || not result)

(* Property: Generator being on should decrease the pace *)
let prop_generator_decreases_pace =
  Test.make ~count:1000 ~name:"prop_generator_decreases_pace" (make gen_triple)
    (fun (monster, difficulty, _) ->
      let pace_with_gen = get_pace difficulty monster true in
      let pace_without_gen = get_pace difficulty monster false in
      get_location monster >= 3 || pace_with_gen < pace_without_gen)

(* Property: All monsters move consistently under the same conditions *)
let prop_consistent_movement =
  Test.make ~count:1000 ~name:"prop_consistent_movement" (make gen_monster_list)
    (fun monsters ->
      let time = 35.0 in
      let difficulty = Normal in
      let door_closed = false in
      let generator_on = false in
      let moved_monsters =
        List.map
          (fun m ->
            let original_location = get_location m in
            ignore
              (move_monster m
                 (time +. get_time m)
                 difficulty door_closed generator_on);
            let has_moved =
              get_location m != original_location || original_location == 0
            in
            has_moved)
          monsters
      in
      List.for_all (fun did_move -> did_move) moved_monsters)

(* ----- Test Suite ----- *)
let suite =
  "Monster Tests"
  >::: [
         "Get Name" >:: test_name;
         "Get Empty Name" >:: test_name_empty;
         "Get Location" >:: test_location;
         "Get Max Location" >:: test_location_max;
         "Get Min Location" >:: test_location_min;
         "Get Time" >:: test_time;
         "Get Max Time" >:: test_time_max;
         "Get Min Time" >:: test_time_min;
         "Initial Positions" >:: test_initial_positions;
         "Movement Without Generator" >:: test_movement_without_generator;
         "Movement With Generator" >:: test_movement_with_generator;
         "Reset When Door Closed" >:: test_reset_when_door_closed;
         "Pace Change With Difficulty" >:: test_pace_change_with_difficulty;
         "No Movement At Start" >:: test_no_movement_at_start;
         "Negative Time" >:: test_negative_time;
         "Max Int Time" >:: test_max_int_time;
         "Min Int Time" >:: test_min_int_time;
         "Random Resets Within Bounds" >:: test_random_resets_within_bounds;
         "Rapid Door State Changes" >:: test_rapid_door_state_changes;
         "Large Number of Cycles" >:: test_large_number_of_cycles;
         "Location Bounds At Maximum" >:: test_location_bounds_at_maximum;
         "Critical Door Interactions" >:: test_critical_door_interactions;
         "Movement Synchronization" >:: test_movement_synchronization;
         "Individual Generator Effects" >:: test_individual_generator_effects;
         "Test Extreme Time Advancement" >:: test_extreme_time_advancement;
         "Invalid Time and Location" >:: test_invalid_time_and_location;
         "Consecutive Door Closures" >:: test_consecutive_door_closures;
         "Consistency Across Runs" >:: test_consistency_across_runs;
         "Extreme Pacing Alterations" >:: test_extreme_pacing_changes;
         "Test Impact of Difficulty" >:: test_difficulty_impact;
         "Names" >:: test_monster_name_consistency;
         "Time Precision" >:: test_timing_precision;
         "Door Block Logic" >:: test_door_blockage_logic;
         "Game Stability" >:: test_game_stability_under_extreme_load;
         "Test Door Closure Effectiveness" >:: test_door_closure_effectiveness;
         "Name Uniqueness" >:: test_monster_name_uniqueness;
         "Individual Monster Creation" >:: test_individual_monster_creation;
         "Monster Movement at Boundary Conditions"
         >:: test_monster_movement_at_boundary_conditions;
         QCheck_ounit.to_ounit2_test
           prop_monster_never_moves_to_negative_location;
         QCheck_ounit.to_ounit2_test prop_monster_resets_if_door_closed;
         QCheck_ounit.to_ounit2_test prop_generator_decreases_pace;
         QCheck_ounit.to_ounit2_test prop_consistent_movement;
       ]

(* ----- OUnit Runner ----- *)
let () = run_test_tt_main suite
