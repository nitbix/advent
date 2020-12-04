open Core;;

let filename = "input.txt" in
let flines = In_channel.read_lines filename in
let entries = List.map flines ~f:Int.of_string in

let rec check a b =
  match a with
  | [] -> None
  | hd_a :: tl_a -> 
      match b with
      | [] -> check tl_a entries
      | hd_b :: tl_b ->
          match hd_a + hd_b with
          | 2020 -> Option.some (hd_a * hd_b)
          | _ -> check a tl_b
in

let r = match (check entries entries) with
| Some x -> Int.to_string x
| None -> "not found"
in 

print_string r
