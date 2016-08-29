open Sys;;
open Unix;;

(* Example As a first example, here is the echo program, which prints a list of
   its arguments, as does the Unix command of the same name. *)
let echo () =
    let len = Array.length Sys.argv in
    print_string Sys.argv.(0);
    print_newline ();
    if len > 1 then
        begin
            print_string Sys.argv.(1);
            for i = 2 to len - 1 do
                print_char ' ';
                print_string Sys.argv.(i);
            done;
            print_newline ();
        end;;
echo ();;
 

let try_finalize f x finally y =
    let res = try f x with exn -> finally y; raise exn in
    finally y;
    res;;


(* execute a function and to restart it automatically 
   when it executes a system call that is interrupted *)
let rec restart_on_EINTR f x =
    try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x;;


(* The function new_timer k f creates a new timer of timer-type k starting the action f, and inactive on creation;
   the function set_timer t sets the value of the timer t (and returns the old value). *)
(*
module Timer : sig
    open Unix
    type t
    val new_timer : interval_timer -> (unit -> unit) -> t
    val get_timer : t -> interval_timer_status
    val set_timer : t -> interval_timer_status -> interval_timer_status
end = struct
    type t = int
    let new_timer timer f : t =
        0
    let get_timer item : interval_timer_status = 
        { it_interval = 0.; it_value = 0.}
    let set_timer timer status : interval_timer_status =
        { it_interval = 0.; it_value = 0.}
end
      *)

exception Break;;

let wait f delay =
   ignore(Sys.signal Sys.sigint (Signal_handle (fun _ -> raise Break)));
   let rec print_and_sleep f delay =
     f ();
     sleep delay;
     print_and_sleep f delay in
   try
     print_endline "enter ";
     print_and_sleep f delay;
   with Break ->
     print_endline "\nexit on Break";
     (fun _ -> 
         ignore(handle_unix_error (fun x -> x / 0) 1);
         print_endline "exit ")();;

wait (fun () -> print_float (time ()); print_newline()) 2;;


let handle_unix_error f arg =
    try
        f arg
    with Unix_error(err, fun_name, arg) ->
        prerr_string Sys.argv.(0);
        prerr_string ": \"";
        prerr_string fun_name;
        prerr_string "\" failed";
        if String.length arg > 0 then begin
            prerr_string " on \"";
            prerr_string arg;
            prerr_string "\""
        end;
        prerr_string ": ";
        prerr_endline (error_message err);
        exit 2;; 

handle_unix_error (fun x -> x / 0) 1;;

