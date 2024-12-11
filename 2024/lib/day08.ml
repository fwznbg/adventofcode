module CharMap = Map.Make(Char)
module CoordSet = Set.Make(struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | anything -> anything
end)

type coord = int * int

let build_map (inputs: string list): coord list CharMap.t =
  let rec iter_cols (tiles: char list) (row_idx: int) (col_idx: int) (acc: coord list CharMap.t): coord list CharMap.t =
    match tiles with
    | [] -> acc
    | tile :: tail ->
      if tile = '.' then iter_cols tail row_idx (col_idx + 1) acc
      else
      let coord = (row_idx, col_idx) in
      let updated_map = if CharMap.mem tile acc then 
        CharMap.add tile (coord :: CharMap.find tile acc) acc
        else CharMap.add tile (coord :: []) acc
      in
      iter_cols tail row_idx (col_idx + 1) updated_map
  in
  let rec iter_rows (inputs: string list) (idx: int) (acc: coord list CharMap.t): coord list CharMap.t =
    match inputs with
    | [] -> acc
    | h :: tail ->
        let updated_map = iter_cols (String.to_seq h |> List.of_seq) idx 0 acc in
        iter_rows tail (idx + 1) updated_map
  in

  iter_rows inputs 0 CharMap.empty

let is_out_of_bound (x: int) (y: int) ((coord_x, coord_y): coord): bool =
  coord_x < 0 || coord_y < 0|| coord_x >= x || coord_y >= y

let add_antinode_part1 (is_out_of_bound: coord -> bool) ((antenna_x, antenna_y): coord) ((delta_x, delta_y): coord) (acc: CoordSet.t): CoordSet.t =
    let antinode = antenna_x + delta_x, antenna_y + delta_y in
    if not (is_out_of_bound antinode) then CoordSet.add antinode acc else acc 

let rec add_antinode_part2 (is_out_of_bound: coord -> bool) ((antenna_x, antenna_y): coord) ((delta_x, delta_y): coord) (acc: CoordSet.t): CoordSet.t =
    let antinode = antenna_x + delta_x, antenna_y + delta_y in
    match is_out_of_bound antinode with
    | true -> acc
    | false -> add_antinode_part2 is_out_of_bound antinode (delta_x, delta_y) (CoordSet.add antinode acc)

let count_unique_antinodes(coord_map: coord list CharMap.t) (is_out_of_bound: coord -> bool) (count_antenna: bool) (add_antinode_fun: (coord -> bool) -> coord -> coord -> CoordSet.t -> CoordSet.t): int =
  let rec aux (cur_coord: coord) (rest_coord: coord list) (acc: CoordSet.t): CoordSet.t =
    let cur_x, cur_y = cur_coord in
    let updated_acc = List.fold_left (fun acc (other_x, other_y)->
      let cur_delta = cur_x - other_x, cur_y - other_y in 
      let other_delta = other_x - cur_x, other_y - cur_y in 
      let add_antinode = add_antinode_fun is_out_of_bound in
      add_antinode cur_coord cur_delta (add_antinode (other_x, other_y) other_delta acc)
    ) acc rest_coord in
    match rest_coord with
    | [] -> updated_acc
    | other :: tail ->
      aux other tail updated_acc
  in
  
  let unique_coords = CharMap.fold (fun _ coords acc -> 
    let coordset = CoordSet.union (aux (List.hd coords) (List.tl coords) CoordSet.empty) acc in
    coordset
  ) coord_map CoordSet.empty in

  let unique_coords = if count_antenna then
    let antenna_set = CharMap.fold (fun _ coords acc->
      List.fold_left (fun acc coord ->
        CoordSet.add coord acc
      ) acc coords
    ) coord_map CoordSet.empty in

    CoordSet.union antenna_set unique_coords
  else unique_coords in

  CoordSet.fold (fun _ acc-> acc + 1) unique_coords 0

let run (inputs: string list): unit =
  let coord_map = build_map inputs in
  let is_out_of_bound_of_grid = is_out_of_bound (List.length inputs) (String.length (List.hd inputs)) in
  let count_unique_antinodes_fun = count_unique_antinodes coord_map is_out_of_bound_of_grid in
  let part1 = count_unique_antinodes_fun false add_antinode_part1 in
  let part2 = count_unique_antinodes_fun true add_antinode_part2 in

  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2;
