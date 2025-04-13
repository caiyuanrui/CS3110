let rec sum = function [] -> 0 | h :: t -> h + sum t
