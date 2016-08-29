open Sys;;
open Unix;;


exception Break;;

type state = { x: float; y: float };;

let state_0 = { x = 0.; y = 0. };;
               
 
let control_cpu_temp state =
  print_endline "state";
  state;;
  
let repeat_until_SIGINT delay state f =
  ignore(signal sigint (Signal_handle (fun _ -> raise Break)));
  let rec repeat delay state f =
    sleep delay;
    repeat delay (f state) f in
  try
    print_endline "enter ";
    repeat delay state f;
  with Break ->
    print_endline "\nexit on Break";;


  
repeat_until_SIGINT  2 state_0 control_cpu_temp;;
