type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid;
               mutable state_index : int * int;
               array_of_possibilities : available Array.t Array.t }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let rec remove_from_list a = function 
  | [] -> []
  | x::xs -> if x=a then remove_from_list a xs else x::remove_from_list a xs

let intersect sez_1 sez_2 = 
  List.filter (fun x -> List.mem x sez_1) sez_2

let komplement sez univerzalna = 
  List.filter (fun x -> not (List.mem x sez)) univerzalna   

let from_opt_arr_to_list array = 
  List.map Option.get (remove_from_list None (Array.to_list array))

(* Zapisali bomo vse možnosti za določeno celico v array arrayov/grid-u *)
let possible_numbers (problem : Model.problem) arr_of_num = 
  let row_to_list i = from_opt_arr_to_list (Model.get_row (problem.initial_grid) i) in
  let column_to_list j = from_opt_arr_to_list (Model.get_column (problem.initial_grid) j) in 
  let box_to_list i j = from_opt_arr_to_list (Model.get_box(problem.initial_grid) ((i / 3) * 3 + (j / 3))) in 
  (* Z zadnjim argumentom dobimo box, ki nas zanima glede na lokacijo*)
  let common i j = intersect (intersect (row_to_list i) (column_to_list j)) box_to_list i j in
  let id = List.init 9 (fun x -> x + 1) in 
  for i = 0 to 0 do
    for j = 0 to 8 do
      arr_of_num.(i).(j) <-
      {loc = (i,j); possible = komplement (common i j) id }
    done
  done

let initialize_state (problem : Model.problem) : state =
  let starting_grid = Array.make_matrix 9 9 [] in
  {problem = Model.copy_grid problem.initial_grid ; current_grid = starting_grid ; state_index = (0,0);
    array_of_possibilities = possible_numbers problem starting_grid}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)

  (* Ideja: za element x bomo ločili dve možnosti, ali je  x rešitev(state_1) ali pa ni(state_2).
            Tako bomo dobili dva state-a. V state_1 vstavimo x in ga os  *)

    let remove_from_row (state : state) i x =  
      for j = 0 to 8 do
        state.array_of_possibilities.(i).(j) 
        <- {loc=(i,j); possible = remove_from_list x state.array_of_possibilities.(i).(j).possible}
      done
    in

    let remove_from_column (state : state) j x =  
      for i = 0 to 8 do
        state.array_of_possibilities.(i).(j) 
        <- {loc=(i,j); possible = remove_from_list x state.array_of_possibilities.(i).(j).possible}
      done
    in

    let remove_from_box (state : state) i j x = 
      let box_i i j = ((i / 3) * 3 + (j / 3)) in 
      let box_i_mod = (box_i i j) mod 3 in 
      for i = (box_i i j) to (box_i i j) + 2 do
        for j = (box_im - 1) to (box_im + 1) do
          state.array_of_possibilities.(i).(j) <- 
          {loc=(i,j); possible = remove_from_list x state.array_of_possibilities.(i).(j).possible}
        done 
      done
    in
    
    let next_index (i, j) = 
      if j < 8 then (i, j + 1) else (i + 1, 0) in 

    (* Začnemo v (i, j), kjer se pojavi prvi None tj. ne zapolnjeno mesto *)
    let (i, j) = 
      let state_index = state.state_index in 
      let rec find_None grid (i, j) = match grid.(i).(j) with
        | None -> (i, j)
        | Some(x) -> find_None grid (next_index (i, j))
    in
      find_None state.current_grid state_index in 
  
    match state.array_of_possibilities.(i).(j).possible with
      | [] -> None
      | x::xs -> 
      let state_1 = {problem = state.problem; current_grid = Model.copy_grid state.current_grid; 
                    state_index = state.state_index, array_of_possibilities = state.array_of_possibilities} in
      let state_2 = {problem = state.problem; current_grid = Model.copy_grid state.current_grid; 
                  state_index = state.state_index, array_of_possibilities = state.array_of_possibilities} in
    
      (* Vstavimo element x v grid *)
      state_1.current_grid.(i).(j) <- Some(x) ;
      (* Posodobimo seznam možnosti, če je x v grid-u *)
      state_1.array_of_possibilities.(i).(j) <- {loc = (i,j); possible = []} ;
      (* S tem odstranimo element x iz seznama moznih stevilk v box, row in column *)
      state_1.array_of_possibilities.(i) <- remove_from_row state i x ;
      state_1.array_of_possibilities <- remove_from_row state j x ;
      state_1.array_of_possibilities <- remove_from_box state i j x ;
      (* Postopek nadaljujemo z naslednjim indekom *) 
      state_1.state_index <- (next_index state_1.state_index) ;
      state_2.array_of_possibilities.(i).(j) <- {loc = (i,j); possible = xs} ;
      Some(state_1, state_2)


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state