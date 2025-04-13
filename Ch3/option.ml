let rec list_max = function
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

let extract = function None -> failwith "panic" | Some i -> i
