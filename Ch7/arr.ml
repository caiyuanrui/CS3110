let v = [| 0; 1 |]
let _ = v.(0) <- 42
let a5 = Array.init 5 (fun _ -> 0)

let _ =
  let i = ref 0 in
  while !i < 5 do
    print_int a5.(!i);
    print_newline ();
    i := !i + 1
  done

let _ =
  for x = 0 to 5 do
    print_int x;
    print_newline ()
  done

let _ =
  for x = 5 downto 0 do
    print_int x;
    print_newline ()
  done
