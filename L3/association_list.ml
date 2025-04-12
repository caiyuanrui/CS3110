let d = [ ("rectangle", 4); ("nonagon", 9); ("icosagon", 20) ]
let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k' = k then Some v else lookup k t
