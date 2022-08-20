(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun stevka -> grid.((box_ind/3)*3 + stevka/3).((box_ind mod 3)*3 + stevka mod 3))

let boxes grid = List.init 9(get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun x -> Array.map f grid.(x))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
         let acc, _ =
           Array.fold_left
             (fun (acc, col_ind) cell ->
                (f row_ind col_ind cell acc, col_ind + 1))
             (acc, 0) row
         in
         (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit = 
  let pomozna = function
    | None -> " "
    | Some(num) -> string_of_int num
  in 
  print_grid pomozna problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)
 
type solution = int grid

let print_solution solution = print_grid string_of_int solution

let intersect sez_1 sez_2 = 
  List.filter (fun x -> List.mem x sez_1) sez_2

let is_valid_solution (problem : problem) (solution : solution) = 
  (* Lažje delati s seznami, zato pretvorimo *)
  let problem_list = Array.to_list (Array.map (Array.to_list) problem.initial_grid) in 
  let solution_list = Array.to_list (Array.map (Array.to_list) solution) in

  (* Preverimo najprej, če solution zadošča pravilnemu gridu(vrstice, stolpci in box-i imajo
  vse različne števke od 1 do 9) in solution ter problem se usejmata *)
  let rec match_rows row_p row_s = match (row_p, row_s) with
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false
    | (x::xs, y::ys) -> (match x with 
        | None -> match_rows xs ys
        | Some (t) -> (t = y) && match_rows xs ys)
  in 
             
  
  let rec match_grids g_p g_s = match (g_p, g_s) with
    | ([], []) -> true
    | (_, []) -> false
    | ([], _) -> false
    | (x::xs, y::ys) -> match_rows x y && match_grids xs ys
  in
        
  
  (*
  let all_numbers solution =
  (* Vsak element solution sekamo z [1; 2;...; 9] in če ima rešitev vse različne števke bo to enako matriki
  iz vrstic [1; 2;...; 9] *)
    let id = List.init 9 (fun x -> x+1) in
    let compare = List.map (fun x -> intersect x id) solution in
    let rec pomozna sez = match sez with 
      | [] -> true
      | x::xs -> if x = id then pomozna xs else false in
    pomozna compare
  in 
  *)

  let all_numbers solution = 
    let id = Array.init 9 (fun x -> x + 1) in 
    Array.for_all (fun x -> Array.exists (fun y -> y = x) solution) (id) 
  in

    
  (List.for_all all_numbers (rows solution)) &&
  (List.for_all all_numbers (columns solution)) &&
  (List.for_all all_numbers (boxes solution)) &&
  (match_grids (problem_list) (solution_list))







type available = { loc : int * int; possible : int list }
  (* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
     želeli imeti še kakšno dodatno informacijo *)
type state = { problem : problem; current_grid : int option grid;
               mutable state_index : int * int;
               array_of_possibilities : available Array.t Array.t }
  
let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid
  
type response = Solved of solution | Unsolved of state | Fail of state
  
let rec remove_from_list a = function 
  | [] -> []
  | x::xs -> if x=a then remove_from_list a xs else x::remove_from_list a xs
  
let intersect sez_1 sez_2 = 
  List.filter (fun x -> List.mem x sez_1) sez_2
  
let komplement sez univerzalna = 
  List.filter (fun x -> not (List.mem x sez)) univerzalna   
  
let from_opt_arr_to_list array = 
  List.map Option.get (remove_from_list None (Array.to_list array))
  
let union sez1 sez2 = 
  let shorty = if (List.length sez1 > List.length sez2) then sez2 else sez1 in
  let longy = if (shorty = sez1) then sez2 else sez1 in
  let rec pom acc shorty = match shorty with
    | [] -> acc
    | x::xs -> if (List.mem x acc) then pom acc xs else pom (x::acc) xs
  in
  pom shorty longy
  
  
  (* Zapisali bomo vse možnosti za določeno celico v array arrayov/grid-u *)
let possible_numbers (problem : problem) arr_of_num = 
  let row_to_list i = from_opt_arr_to_list (get_row (problem.initial_grid) i) in
  let column_to_list j = from_opt_arr_to_list (get_column (problem.initial_grid) j) in 
  let box_to_list i j = from_opt_arr_to_list (get_box   (problem.initial_grid) ((i / 3) * 3 + (j / 3))) in 
  (* Z zadnjim argumentom dobimo box, ki nas zanima glede na lokacijo*)
  let get_union i j = union (union (row_to_list i) (column_to_list j)) (box_to_list i j) in
  let id = List.init 9 (fun x -> x + 1) in 
  for i = 0 to 8 do
    for j = 0 to 8 do
      arr_of_num.(i).(j) <-
        {loc = (i,j); possible = komplement (get_union i j) id }
    done
  done;
  Printf.printf "possible_numbers" ;
  arr_of_num 
  
let initialize_state (problem : problem) : state =
  let starting_grid = Array.make_matrix 9 9 {loc = (0,0); possible = ([] : int list)} in
  {problem = problem ; current_grid = problem.initial_grid ; state_index = (0,0);
   array_of_possibilities = possible_numbers problem starting_grid}
  
let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid in
  if unsolved then Unsolved state
  else
  (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
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
    let box_i = ((i / 3) * 3 + (j / 3)) in 
    let box_i_mod = box_i mod 3  in 
    for i = (box_i_mod) to (box_i_mod + 2) do
      for j = (box_i_mod) to (box_i_mod + 2) do
        state.array_of_possibilities.(i).(j) <- 
          {loc=(i,j); possible = remove_from_list x state.array_of_possibilities.(i).(j).possible}
      done 
    done 
  in
      
  let next_index (i, j) = 
    if i < 8 then
      (if j < 8 then (i, j + 1) else (i + 1, 0)) else 
    if i = 8 then (if j < 8 then (i, j + 1) else (0, 0)) else (0, 0)
  in
  
  (* Začnemo v (i, j), kjer se pojavi prvi None tj. ne zapolnjeno mesto *)
  let (i, j) = 
    let state_index = state.state_index in 
    let rec find_None grid (i, j) = match grid.(i).(j) with
      | None -> (i, j)
      | Some(x) -> find_None grid (next_index (i, j))
    in 
    find_None state.current_grid state_index 
  in 
    
  match state.array_of_possibilities.(i).(j).possible with
  | [] -> None
  | x::xs -> 
      let state_1 = {problem = state.problem; current_grid = copy_grid state.current_grid; 
                     state_index = state.state_index; array_of_possibilities = copy_grid state.array_of_possibilities} in
      let state_2 = {problem = state.problem; current_grid = copy_grid state.current_grid; 
                     state_index = state.state_index; array_of_possibilities = copy_grid state.array_of_possibilities} in
      
    (* Vstavimo element x v grid *)
      state_1.current_grid.(i).(j) <- Some(x) ;
    (* Posodobimo seznam možnosti, če je x v grid-u *)
      state_1.array_of_possibilities.(i).(j) <- {loc = (i,j); possible = []} ;
    (* S tem odstranimo element x iz seznama moznih stevilk v box, row in column *)
      remove_from_row state_1 i x ;
      remove_from_column state_1 j x ;
      remove_from_box state_1 i j x ;
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
  
let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state

let read_problem filename =
  let channel = open_in filename in
  let str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  problem_of_string str
  
let find_solution problem =
  let before = Sys.time () in
  let solution = solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)
  
let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"

let find_and_display_solution (problem : problem) =
  Printf.printf "Rešujem:\n";
  print_problem problem;
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time

let () =
  (* Če se program sesuje, nam to izpiše klicni sklad. *)
  Printexc.record_backtrace true;
  (* Tabela sistemskih argumentov vsebuje ime klicanega programa ter argumente, ki mu sledijo *)
  Sys.argv
  (* Tabelo pretvorimo v seznam *)
  |> Array.to_list
  (* Odstranimo prvi element (ime klicanega programa), da dobimo seznam imen datotek *)
  |> List.tl
  (* Iz vsake datoteke preberemo problem *)
  |> List.map read_problem
  (* Probleme zaporedoma rešimo *)
  |> List.iter find_and_display_solution
    
    
let () = "
┏━━━┯━━━┯━━━┓
┃4 3│921│65 ┃
┃967│3 5│82 ┃
┃ 51│876│49 ┃
┠───┼───┼───┨
┃548│132│976┃
┃ 29│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃ 72│689│514┃
┃814│25 │769┃
┃  5│417│382┃
┗━━━┷━━━┷━━━┛"
         |> problem_of_string
         |> find_and_display_solution 