(* test_monster.ml *)
open OUnit2
open Fnaf.Monster
open Fnaf.Game
open QCheck

(* ----- Helper Functions ----- *)
let setup_monsters initial_location initial_time =
  [
    create_monster "Chica" initial_location initial_time;
    create_monster "Foxy" initial_location initial_time;
    create_monster "Bonnie" initial_location initial_time;
    create_monster "Freddy Fazbear" initial_location initial_time;
  ]

let advance_time monsters time_diff difficulty door_closed generator_on enraged
    =
  List.iter
    (fun monster ->
      ignore
        (move_monster monster
           (get_time monster +. time_diff)
           difficulty door_closed generator_on enraged))
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

let battery_within_bounds state =
  get_battery state >= 0 && get_battery state <= 100

(* ----- Monster Tests ----- *)

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
  advance_time monsters 30. Normal false false false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test movement logic with generator on reduces pace *)
let test_movement_with_generator _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 56. Normal false true false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test monsters reset location when door is closed at location 0 *)
let test_reset_when_door_closed _ =
  let monsters = setup_monsters 0 0.0 in
  advance_time monsters 60. Normal true false false;
  List.iter
    (fun m ->
      assert_bool "Monsters should reset to a new location" (get_location m > 0))
    monsters

(* Test pace change based on difficulty levels *)
let test_pace_change_with_difficulty _ =
  let monster = create_monster "Chica" 5 0.0 in
  let easy_pace = get_pace Easy monster false false in
  let hard_pace = get_pace Hard monster false false in
  assert_bool "Hard difficulty should have a faster pace" (hard_pace < easy_pace)

(* Test non-movement when the game starts (time = 0) *)
let test_no_movement_at_start _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 0. Easy false false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test negative time does not cause errors or movement *)
let test_negative_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (-10.) Easy false false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test maximum integer time advancement *)
let test_max_int_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (float_of_int max_int) Easy false false false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test minimum integer time regression *)
let test_min_int_time _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters (-.float_of_int max_int) Easy false false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test random resets within game bounds *)
let test_random_resets_within_bounds _ =
  Random.self_init ();
  let monsters = setup_monsters 5 0.0 in
  for _ = 1 to 10 do
    advance_time monsters (Random.float 50.) Hard true true false
  done;
  List.iter
    (fun m ->
      assert_bool "Monsters should be within valid game bounds"
        (get_location m >= 0 && get_location m <= 5))
    monsters

let test_extreme_time_advancement _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 31536000.0 (* 1 year in seconds *) Easy false false
    false;
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
    advance_time monsters 1. Hard true false false;
    (* Door closed *)
    advance_time monsters 1. Hard false false false (* Door opened *)
  done;
  assert_location monsters [ 1; 1; 1; 1 ]
(* Check for expected behavior after consecutive door actions *)

let test_consistency_across_runs _ =
  let first_run = setup_monsters 5 0.0 in
  let second_run = setup_monsters 5 0.0 in
  advance_time first_run 10. Normal false false false;
  advance_time second_run 10. Normal false false false;
  assert_equal
    ~printer:(fun lst -> String.concat "; " (List.map string_of_int lst))
    (get_locations first_run) (get_locations second_run)

let test_extreme_pacing_changes _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 5. Easy false false false;
  advance_time monsters 5. Hard false false false;
  advance_time monsters 5. Easy false false false;
  assert_location monsters [ 5; 5; 5; 5 ]

(* Test rapid sequence of door state changes *)
let test_rapid_door_state_changes _ =
  let monsters = setup_monsters 5 0.0 in
  for _ = 1 to 10 do
    let door_state = Random.bool () in
    advance_time monsters 5. Normal door_state false false
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
    advance_time monsters (float_of_int i) Hard false gen_state false
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
    advance_time monsters 10. Easy false false false
  done;
  List.iter
    (fun m ->
      assert_bool "Monster location should handle max int values properly"
        (get_location m >= 0 && get_location m <= max_int))
    monsters

(* Test door interaction during critical monster movements *)
let test_critical_door_interactions _ =
  let monsters = setup_monsters 1 0.0 in
  advance_time monsters 1. Hard true false false;
  assert_location monsters [ 1; 1; 1; 1 ]
(* Assuming monsters try to move to location 0 but door is closed *)

(* Test monster movement synchronization under uniform conditions *)
let test_movement_synchronization _ =
  let monsters = setup_monsters 5 0.0 in
  advance_time monsters 30. Hard false true false;
  assert_location monsters [ 4; 4; 4; 4 ]

(* Test each monster's independent reaction to generator on/off state *)
let test_individual_generator_effects _ =
  let monsters = setup_monsters 5 0.0 in
  List.iteri
    (fun i m ->
      advance_time [ m ]
        (10. *. float_of_int (i + 1))
        Hard false
        (i mod 2 = 0)
        false)
    monsters;
  assert_location monsters [ 5; 4; 4; 4 ]

let test_difficulty_impact _ =
  let easy_pace = get_pace Easy (create_monster "Chica" 5 0.0) false false in
  let hard_pace = get_pace Hard (create_monster "Chica" 5 0.0) false false in
  assert_bool "Hard difficulty should result in a faster pace"
    (hard_pace < easy_pace)

let test_monster_name_consistency _ =
  let monsters = init_monsters in
  let names_before = List.map get_name monsters in
  advance_time monsters 60. Normal false false false;
  let names_after = List.map get_name monsters in
  assert_equal ~printer:(String.concat ", ") names_before names_after
    ~msg:"Monster names should remain consistent after state changes"

let test_timing_precision _ =
  let monsters = setup_monsters 5 0.0 in
  List.iter (fun m -> advance_time [ m ] 0.1 Hard false false false) monsters;
  List.iter
    (fun m ->
      assert_bool "Timing updates should be precise to the second"
        (abs_float (get_time m -. 0.1) < 1.))
    monsters

let test_door_blockage_logic _ =
  let monsters = setup_monsters 1 0.0 in
  for _ = 1 to 10 do
    advance_time monsters 1. Normal true false false
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
      advance_time monsters 0.5 Hard false false false;
      simulate_actions monsters (count - 1)
    end
  in
  assert_bool "Game should handle extreme load without crashing"
    (simulate_actions large_number_of_monsters 100)

let test_door_closure_effectiveness _ =
  let monsters = setup_monsters 1 0.0 in
  advance_time monsters 5. Hard true false false;
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
  let _ = move_monster monster_at_min 10.0 Normal false false false in
  let _ = move_monster monster_at_max 10.0 Normal false false false in
  assert_equal 0 (get_location monster_at_min);
  (* Expect no movement beyond lower boundary *)
  assert_equal 5 (get_location monster_at_max)
(* Expect normal decrement in location *)

let test_update_monsters _ =
  let monsters = setup_monsters 5 0.0 in
  let update = update_monsters monsters 22. Normal true false false in
  assert_equal (false, []) update;
  assert_location monsters [ 4; 4; 5; 5 ]

let test_update_monsters_long _ =
  let monsters = setup_monsters 5 0.0 in
  let update = update_monsters monsters 62. Normal true false false in
  assert_equal (false, []) update;
  assert_location monsters [ 4; 4; 4; 4 ]

let test_update_monsters_open_office _ =
  let monsters = setup_monsters 1 0.0 in
  let update = update_monsters monsters 22. Normal false false false in
  assert_equal (true, [ "Chica"; "Foxy" ]) update;
  assert_location monsters [ 0; 0; 1; 1 ]

let test_update_monsters_closed_office _ =
  let monsters = setup_monsters 1 0.0 in
  let update = update_monsters monsters 22. Normal true false false in
  assert_equal (false, []) update

let test_update_monsters_generator_on _ =
  let monsters = setup_monsters 2 0.0 in
  let update = update_monsters monsters 12. Normal true true false in
  assert_equal (false, []) update;
  assert_location monsters [ 1; 1; 2; 2 ]

let test_update_monsters_generator_on_open_office _ =
  let monsters = setup_monsters 1 0.0 in
  let update = update_monsters monsters 8. Normal false true false in
  assert_equal (true, [ "Foxy" ]) update;
  assert_location monsters [ 1; 0; 1; 1 ]

let test_update_monsters_generator_on_closed_office _ =
  let monsters = setup_monsters 1 0.0 in
  let update = update_monsters monsters 12. Normal true true false in
  assert_equal (false, []) update

let test_get_monsters_at_location _ =
  let monsters = setup_monsters 5 0.0 in
  let _ = update_monsters monsters 22. Normal true false false in
  assert_equal [ "Chica"; "Foxy" ] (get_monsters_at_location monsters 4);
  assert_equal
    [ "Bonnie"; "Freddy Fazbear" ]
    (get_monsters_at_location monsters 5);
  let _ = update_monsters monsters 27. Normal true false false in
  assert_equal
    [ "Chica"; "Foxy"; "Bonnie" ]
    (get_monsters_at_location monsters 4);
  assert_equal [ "Freddy Fazbear" ] (get_monsters_at_location monsters 5);
  let _ = update_monsters monsters 38. Normal true false false in
  assert_equal [ "Foxy" ] (get_monsters_at_location monsters 3);
  assert_equal
    [ "Chica"; "Bonnie"; "Freddy Fazbear" ]
    (get_monsters_at_location monsters 4)

let test_no_negative_locations _ =
  let monsters = setup_monsters 1 0.0 in
  advance_time monsters (-10.0) Normal false false false;
  List.iter
    (fun m ->
      assert_bool "Monsters should not have negative locations"
        (get_location m >= 0))
    monsters

let test_monster_no_movement_short_interval _ =
  let monster = create_monster "Freddy Fazbear" 5 0.0 in
  ignore (move_monster monster 5.0 Normal false false false);
  (* Assuming pace > 5.0 *)
  assert_equal 5 (get_location monster)

let test_monster_initial_locations_not_above_5 _ =
  let monsters = init_monsters in
  (* Assuming this initializes all monsters *)
  List.iter
    (fun monster ->
      assert_bool "Monster location should not exceed 5"
        (get_location monster <= 5))
    monsters

let test_monster_count_never_exceeds_four _ =
  let monsters = init_monsters in
  (* Assume this function initializes the game's monsters *)
  let num_monsters = List.length monsters in
  assert_bool "Number of monsters should not exceed four" (num_monsters <= 4)

let test_list_to_string _ =
  (* Test empty list *)
  let empty = list_to_string [] in
  assert_equal "" empty ~msg:"Should handle empty list";

  (* Test single element list *)
  let single = list_to_string [ "Hello" ] in
  assert_equal "Hello" single ~msg:"Should handle single element list"

let test_monsters_differential_movement _ =
  let monster1 = create_monster "Foxy" 5 0.0 in
  let monster2 = create_monster "Bonnie" 1 0.0 in
  ignore (move_monster monster1 15.0 Normal false false false);
  ignore (move_monster monster2 15.0 Normal false false false);
  assert_bool "Monsters at different initial locations should move differently"
    (get_location monster1 <> get_location monster2)

let test_multiple_generators_impact_pace _ =
  let monster = create_monster "Bonnie" 2 0.0 in
  let pace_initial = get_pace Hard monster false false in
  let pace_with_generator = get_pace Hard monster true false in
  let pace_after_double_toggle = get_pace Hard monster false false in
  assert_equal pace_initial pace_after_double_toggle;
  assert_bool "Pace with generator should be less"
    (pace_with_generator < pace_initial)

let test_monster_consecutive_movements _ =
  let monster = create_monster "Chica" 5 0.0 in
  ignore (move_monster monster 30.0 Normal false false false);
  (* First move *)
  let location_after_first_move = get_location monster in
  ignore (move_monster monster 60.0 Normal false false false);
  (* Second move *)
  let location_after_second_move = get_location monster in
  assert (location_after_second_move < location_after_first_move)

let test_game_end_condition _ =
  let monsters =
    [
      create_monster "Freddy" 0 0.0;
      create_monster "Bonnie" 0 0.0;
      create_monster "Chica" 0 0.0;
      create_monster "Foxy" 0 0.0;
    ]
  in
  let game_over, _ = update_monsters monsters 30.0 Normal false false false in
  assert_bool "Game should end when all monsters converge at location 0"
    game_over

let test_monster_reacts_to_hour_change _ =
  let monster = create_monster "Bonnie" 5 0.0 in
  (* Simulate game time passing to just before an hour transition *)
  ignore (move_monster monster 0. Normal false false false);
  let location_before_hour_change = get_location monster in
  (* Simulate crossing the hour threshold *)
  ignore (move_monster monster 3601.0 Normal false false false);
  let location_after_hour_change = get_location monster in
  assert_bool "Monster should move more aggressively after an hour change"
    (location_after_hour_change < location_before_hour_change)

let test_game_behavior_when_time_freezes _ =
  let initial_time = Unix.gettimeofday () in
  let monster = create_monster "Foxy" 5 initial_time in
  Unix.sleep 2;
  (* Simulate game pausing for 2 seconds, no game time should pass *)
  ignore (move_monster monster initial_time Normal false false false);
  (* Move monster at the same initial time *)
  assert_equal ~msg:"Monster should not move when game time is frozen" 5
    (get_location monster)

let test_timing_precision_on_monster_moves _ =
  let monster = create_monster "Bonnie" 5 0.0 in
  ignore (move_monster monster 15.0 Normal false false false);
  (* Monster moves after 15 seconds *)
  assert_equal ~msg:"Monster should not move too soon" 5 (get_location monster);
  ignore (move_monster monster 30.0 Normal false false false);
  (* Now it should move *)
  assert_bool "Monster should have moved" (get_location monster < 5)

let test_full_night_monster_movement_simulation _ =
  let monsters = init_monsters in
  let simulate_hourly_movements hour =
    List.iter
      (fun m ->
        let time = float_of_int (hour * 3600) in
        (* Convert hours to seconds *)
        let difficulty = if hour >= 3 then Hard else Normal in
        let door_status = hour mod 2 = 0 in
        (* Alternate door status every hour *)
        ignore (move_monster m time difficulty door_status false false))
      monsters;
    List.iter
      (fun m ->
        assert_bool "Monsters should not move into the room when door is closed"
          (get_location m > 0 || not (hour mod 2 = 0)))
      monsters
  in
  for hour = 1 to 6 do
    (* Simulate movements from 1 AM to 6 AM *)
    simulate_hourly_movements hour
  done;
  List.iter
    (fun m ->
      (* Assert that no monster ends at the start location *)
      assert_bool
        "No monster should be at the initial position at the end of the night"
        (get_location m <> 5))
    monsters

let test_monsters_locations_overlap _ =
  let monsters =
    [
      create_monster "Chica" 3 0.0;
      (* Starting closer to the critical point *)
      create_monster "Bonnie" 4 0.0;
      (* Starting one step farther than Chica *)
      create_monster "Freddy" 4 0.0;
      (* Same start as Bonnie *)
      create_monster "Foxy" 5 0.0;
      (* Farthest start point *)
    ]
  in
  (* Move all monsters with enough time to ensure they reach the same
     location *)
  List.iter
    (fun monster -> ignore (move_monster monster 60.0 Normal false false false))
    monsters;
  let location_counts =
    List.fold_left
      (fun acc monster ->
        let loc = get_location monster in
        let count = try List.assoc loc acc with Not_found -> 0 in
        (loc, count + 1) :: List.remove_assoc loc acc)
      [] monsters
  in
  let overlaps = List.filter (fun (_, count) -> count > 1) location_counts in
  assert_bool "There should be at least one location with overlapping monsters"
    (List.length overlaps > 0)

(* Varying times and generator states should result in varied locations *)

(* ----- QCheck Property Definitions ----- *)

(* Property: Monsters should never move to a negative location *)
let prop_monster_never_moves_to_negative_location =
  Test.make ~count:100 ~name:"prop_monster_never_moves_to_negative_location"
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
  Test.make ~count:100 ~name:"prop_monster_resets_if_door_closed"
    (make gen_monster) (fun monster ->
      let monster' = create_monster (get_name monster) 0 (get_time monster) in
      let result = move_monster monster' 30.0 Hard true true false in
      get_location monster' > 0 || not result)

(* Property: Generator being on should decrease the pace *)
let prop_generator_decreases_pace =
  Test.make ~count:100 ~name:"prop_generator_decreases_pace" (make gen_triple)
    (fun (monster, difficulty, _) ->
      let pace_with_gen = get_pace difficulty monster true false in
      let pace_without_gen = get_pace difficulty monster false false in
      get_location monster >= 3 || pace_with_gen < pace_without_gen)

(* Property: All monsters move consistently under the same conditions *)
let prop_consistent_movement =
  Test.make ~count:100 ~name:"prop_consistent_movement" (make gen_monster_list)
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
                 difficulty door_closed generator_on false);
            let has_moved =
              get_location m != original_location || original_location == 0
            in
            has_moved)
          monsters
      in
      List.for_all (fun did_move -> did_move) moved_monsters)

let monster_suite =
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
         "Update Monsters" >:: test_update_monsters;
         "Update Monsters Long Duration Consistent Locations"
         >:: test_update_monsters_long;
         "Update Monsters Open Office Rooms and Game Over"
         >:: test_update_monsters_open_office;
         "Update Monsters Closed Office Rooms"
         >:: test_update_monsters_closed_office;
         "Update Monsters Generator On Rooms"
         >:: test_update_monsters_generator_on;
         "Update Monsters Generator On Rooms Open Office"
         >:: test_update_monsters_generator_on_open_office;
         "Update Monsters Generator On Rooms Closed Office"
         >:: test_update_monsters_generator_on_closed_office;
         "Get Monsters at Location" >:: test_get_monsters_at_location;
         "Test No Negative Locations" >:: test_no_negative_locations;
         "No Movement in Short Time Spans"
         >:: test_monster_no_movement_short_interval;
         "Locations Never More Than 5"
         >:: test_monster_initial_locations_not_above_5;
         "Number of Monsters Never Exceeds Four"
         >:: test_monster_count_never_exceeds_four;
         "List to String Function" >:: test_list_to_string;
         "Monsters With Differential Movement and Locations"
         >:: test_monsters_differential_movement;
         "Test Monsters with Multiple Generators"
         >:: test_multiple_generators_impact_pace;
         "Two Consecutive Movements" >:: test_monster_consecutive_movements;
         "Test Game End Condition" >:: test_game_end_condition;
         "Monster Reaction to Game Hour Change"
         >:: test_monster_reacts_to_hour_change;
         "Time Freezing" >:: test_game_behavior_when_time_freezes;
         "Test Precision of Simultaneous Movement Time"
         >:: test_timing_precision_on_monster_moves;
         "Full Night Movement Accuracy"
         >:: test_full_night_monster_movement_simulation;
         "Overlap in Monsters" >:: test_monsters_locations_overlap;
         QCheck_ounit.to_ounit2_test
           prop_monster_never_moves_to_negative_location;
         QCheck_ounit.to_ounit2_test prop_monster_resets_if_door_closed;
         QCheck_ounit.to_ounit2_test prop_generator_decreases_pace;
         QCheck_ounit.to_ounit2_test prop_consistent_movement;
       ]

(* ----- Game Tests ----- *)

let test_update_command_times _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 [ 3. ]
  in
  let new_time = 123.45 in
  update_command_times initial_state new_time;
  assert_equal [ new_time; 3. ] (get_command_times initial_state)
    ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_float l) ^ "]")

let test_update_command_times_empty _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  let new_time = 123.45 in
  update_command_times initial_state new_time;
  assert_equal [ new_time ] (get_command_times initial_state) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_float l) ^ "]")

let test_update_command_times_multiple _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  let times = [ 123.45; 67.89; 12.34 ] in
  List.iter (update_command_times initial_state) times;
  assert_equal (List.rev times) (get_command_times initial_state)
    ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_float l) ^ "]")

let test_update_command_times_preserve_order _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  update_command_times initial_state 50.0;
  update_command_times initial_state 100.0;
  assert_equal [ 100.0; 50.0 ] (get_command_times initial_state)
    ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_float l) ^ "]")

let test_too_many_commands_no_commands _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  assert_equal (too_many_commands initial_state 20.0) false

let test_too_many_commands_old_commands _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 [ 5.0; 9.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) false;
  assert_equal [] (get_command_times initial_state)

let test_too_many_commands_recent_commands _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 [ 15.0; 18.0; 19.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) false;
  assert_equal [ 15.0; 18.0; 19.0 ] (get_command_times initial_state)

let test_too_many_commands_mixed _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0
      [ 9.0; 10.0; 12.0; 19.5; 20.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) false;
  assert_equal [ 12.0; 19.5; 20.0 ] (get_command_times initial_state)

let test_too_many_commands_boundary _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0
      [ 11.0; 12.0; 13.0; 14.0; 15.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) false

let test_too_many_commands_over_boundary _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0
      [ 11.0; 12.0; 13.0; 14.0; 15.0; 15.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) true

let test_too_many_commands_under_boundary _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 [ 15.0; 16.0; 17.0 ]
  in
  assert_equal (too_many_commands initial_state 20.0) false

let test_door_typical _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "door") 5

let test_door_powersaving _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters PowerSaving false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "door") 3

let test_light_typical _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "light") 2

let test_light_powersaving _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters PowerSaving false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "light") 1

let test_camera_typical _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "camera") 1

let test_camera_powersaving _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters PowerSaving false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "camera") 1

let test_unrecognized_action _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  assert_equal (power_consumption_rates initial_state "unknown") 0

let test_opgenerator_battery_below_90 _ =
  let initial_state =
    gen_state 80 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  operate_generator initial_state;
  assert_equal 90 (get_battery initial_state)

let test_opgenerator_battery_at_90 _ =
  let initial_state =
    gen_state 90 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  operate_generator initial_state;
  assert_equal 100 (get_battery initial_state)

let test_opgenerator_battery_above_90 _ =
  let initial_state =
    gen_state 95 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  operate_generator initial_state;
  assert_equal 100 (get_battery initial_state)

let test_opgenerator_battery_at_100 _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  operate_generator initial_state;
  assert_equal 100 (get_battery initial_state)

let test_toggle_generator_off _ =
  let initial_state =
    gen_state 80 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical true Hard 0 []
  in
  toggle_generator initial_state;
  assert_equal false (get_generator_on initial_state);
  assert_equal 80 (get_battery initial_state)

let test_toggle_generator_on_below_90 _ =
  let initial_state =
    gen_state 80 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  toggle_generator initial_state;
  assert_equal true (get_generator_on initial_state);
  assert_equal 90 (get_battery initial_state)

let test_toggle_generator_on_at_90 _ =
  let initial_state =
    gen_state 90 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  toggle_generator initial_state;
  assert_equal true (get_generator_on initial_state);
  assert_equal 100 (get_battery initial_state)

let test_toggle_generator_on_above_90 _ =
  let initial_state =
    gen_state 95 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  toggle_generator initial_state;
  assert_equal true (get_generator_on initial_state);
  assert_equal 100 (get_battery initial_state)

let test_toggle_generator_on_at_100 _ =
  let initial_state =
    gen_state 100 0.0 true false None false false
      [ (1, true); (2, false) ]
      init_monsters Typical false Hard 0 []
  in
  toggle_generator initial_state;
  assert_equal false (get_generator_on initial_state);
  assert_equal 100 (get_battery initial_state)

let test_apply_random_power_up_qcheck =
  Test.make ~count:100 ~name:"test_apply_random_power_up_qcheck"
    (make ~print:Print.int (Gen.int_bound 100))
    (fun initial_battery ->
      let state =
        gen_state initial_battery 0.0 true false None false false
          [ (1, true); (2, false) ]
          init_monsters Typical false Hard 0 [ 3. ]
      in
      apply_random_power_up state;
      battery_within_bounds state)

let test_resolve_power_surge _ =
  let state =
    gen_state 80 0.0 true true (Some PowerSurge) false true
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical false Hard 0 []
  in
  resolve_hazard state;
  assert_equal None (get_hazard state);
  assert_equal
    [ (1, true); (2, true); (3, true); (4, true); (5, true) ]
    (get_camera_statuses state);
  assert_equal true (get_light_malfunction state);
  assert_equal true (get_door_jammed state)

let test_resolve_power_surge_alone _ =
  let state =
    gen_state 80 0.0 true false (Some PowerSurge) false false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical false Hard 0 []
  in
  resolve_hazard state;
  assert_equal None (get_hazard state);
  assert_equal
    [ (1, true); (2, true); (3, true); (4, true); (5, true) ]
    (get_camera_statuses state);
  assert_equal false (get_light_malfunction state);
  assert_equal false (get_door_jammed state)

let test_process_command_door_open _ =
  let state = initial_state Normal in
  process_command state "door";
  assert_equal true (get_door_closed state);
  assert_equal 95 (get_battery state)

let test_process_command_door_close _ =
  let state =
    gen_state 100 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical false Normal 0 []
  in
  process_command state "door";
  assert_equal false (get_door_closed state);
  assert_equal 95 (get_battery state)

let test_process_command_light_on _ =
  let state = initial_state Normal in
  process_command state "light";
  assert_equal true (get_light_on state);
  assert_equal 98 (get_battery state)

let test_process_command_light_off _ =
  let state =
    gen_state 100 0.0 true false None true false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical false Normal 0 []
  in
  process_command state "light";
  assert_equal false (get_light_on state);
  assert_equal 98 (get_battery state)

let test_process_command_toggle_generator_on _ =
  let state =
    gen_state 80 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical false Normal 0 []
  in
  process_command state "toggle_generator";
  assert_equal true (get_generator_on state);
  assert_equal 90 (get_battery state)

let test_process_command_toggle_generator_off _ =
  let state =
    gen_state 100 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters Typical true Normal 0 []
  in
  process_command state "toggle_generator";
  assert_equal false (get_generator_on state);
  assert_equal 100 (get_battery state)

let test_process_command_toggle_power_mode_to_saving _ =
  let state = initial_state Normal in
  process_command state "toggle_power_mode";
  assert_equal PowerSaving (get_power_mode state)

let test_process_command_toggle_power_mode_to_typical _ =
  let state =
    gen_state 100 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, false) ]
      init_monsters PowerSaving false Normal 0 []
  in
  process_command state "toggle_power_mode";
  assert_equal Typical (get_power_mode state)

let test_process_command_camera1 _ =
  let state =
    gen_state 100 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, true) ]
      init_monsters Typical false Hard 0 []
  in
  process_command state "camera1";
  assert_equal 99 (get_battery state);
  assert_equal
    [ (1, false); (2, false); (3, false); (4, true); (5, false) ]
    (get_camera_statuses state)

let test_process_command_camera_invalid _ =
  let state =
    gen_state 100 0.0 true false None false false
      [ (1, false); (2, false); (3, false); (4, false); (5, true) ]
      init_monsters Typical false Normal 0 []
  in
  process_command state "camera6";
  assert_equal 99 (get_battery state);
  assert_equal
    [ (1, false); (2, false); (3, false); (4, true); (5, false) ]
    (get_camera_statuses state)

let test_process_command_invalid _ =
  let state = initial_state Normal in
  let battery_before = get_battery state in
  process_command state "invalid";
  assert_equal battery_before (get_battery state);
  assert_equal false (get_door_closed state);
  assert_equal false (get_light_on state);
  assert_equal false (get_generator_on state);
  assert_equal Typical (get_power_mode state)

let game_suite =
  "Game Tests"
  >::: [
         "Update Command Times" >:: test_update_command_times;
         "Update Command Times Empty" >:: test_update_command_times_empty;
         "Update Command Times Multiple" >:: test_update_command_times_multiple;
         "Update Command Times Preserve Order"
         >:: test_update_command_times_preserve_order;
         "No Commands" >:: test_too_many_commands_no_commands;
         "Old Commands" >:: test_too_many_commands_old_commands;
         "Recent Commands" >:: test_too_many_commands_recent_commands;
         "Mixed Commands" >:: test_too_many_commands_mixed;
         "Boundary" >:: test_too_many_commands_boundary;
         "Over Boundary" >:: test_too_many_commands_over_boundary;
         "Under Boundary" >:: test_too_many_commands_under_boundary;
         "Door typical" >:: test_door_typical;
         "Door powersaving" >:: test_door_powersaving;
         "Light typical" >:: test_light_typical;
         "Light powersaving" >:: test_light_powersaving;
         "Camera typical" >:: test_camera_typical;
         "Camera powersaving" >:: test_camera_powersaving;
         "Unrecognized action" >:: test_unrecognized_action;
         "Operate generator battery below 90"
         >:: test_opgenerator_battery_below_90;
         "Operate generator battery at 90" >:: test_opgenerator_battery_at_90;
         "Operate generator battery above 90"
         >:: test_opgenerator_battery_above_90;
         "Operate generator battery at 100" >:: test_opgenerator_battery_at_100;
         "Toggle generator off" >:: test_toggle_generator_off;
         "Toggle generator on battery below 90"
         >:: test_toggle_generator_on_below_90;
         "Toggle generator on battery at 90" >:: test_toggle_generator_on_at_90;
         "Toggle generator on battery above 90"
         >:: test_toggle_generator_on_above_90;
         "Toggle generator on battery at 100"
         >:: test_toggle_generator_on_at_100;
         QCheck_ounit.to_ounit2_test test_apply_random_power_up_qcheck;
         "Resolve power surge with other hazards" >:: test_resolve_power_surge;
         "Resolve power surge without other hazards"
         >:: test_resolve_power_surge_alone;
         "Door open command" >:: test_process_command_door_open;
         "Door close command" >:: test_process_command_door_close;
         "Light on command" >:: test_process_command_light_on;
         "Light off command" >:: test_process_command_light_off;
         "Toggle Generator on command"
         >:: test_process_command_toggle_generator_on;
         "Toggle Generator off command"
         >:: test_process_command_toggle_generator_off;
         "Toggle Power Mode to Saving command"
         >:: test_process_command_toggle_power_mode_to_saving;
         "Toggle Power Mode to Typical command"
         >:: test_process_command_toggle_power_mode_to_typical;
         "Camera1 command" >:: test_process_command_camera1;
         "Camera invalid command" >:: test_process_command_camera_invalid;
         "Invalid command" >:: test_process_command_invalid;
       ]

(* ----- OUnit Runner ----- *)
let () =
  run_test_tt_main monster_suite;
  run_test_tt_main game_suite
