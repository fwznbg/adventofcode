let read_lines (filename: string) : string list = 
  let lines = ref [] in
  let ic = open_in filename in 
  try
    while true; do
      lines := input_line ic :: !lines
    done;
  with End_of_file -> 
    close_in ic;
    List.rev !lines


let () = 
    read_lines "./input/day04" |> Aoc2024.Day04.run
