open Sys;;
open Unix;;
open Format;;
open Core.Std;; (* M-x merlin-use *)
open Str;;      (* corebuild -pkg str cpu.native *)
  
exception Break;;



(* Utilities *)  

  
let read_file converter filename =
  let file = In_channel.create filename in
  let content =
    file
    |> In_channel.input_lines 
    |> List.map ~f:converter 
    |> Array.of_list
  in
  In_channel.close file;
  content
;;


let write_file filename f =
  let outc = Out_channel.create filename in
  f outc;
  Out_channel.close outc
;;

  
let split s = s |> Str.split (Str.regexp_string " ") ;;  


let choose_from_ascending array value =
  let last = (Array.length array) - 1 in 
  if value <= array.(0) || last = 0 then array.(0)
  else ( if value >= array.(last) then array.(last)
         else (
           let i = ref 0 in
           let res = ref 0. in
           while !i < last-1 do
             if value >= array.(!i) && value <= array.(!i+1) then (
               res := if value <= (array.(!i+1) -. array.(!i)) /. 2. then array.(!i) else array.(!i+1);
               i := last;
             )
             else
               i := !i + 1;
           done;
           !res
         )
       )
;;     


  
(* CPU interfaces for Debian/Ubuntu, verified on Pine64 on DietPi *)

  
(*
 # Cpu Governor | ondemand | powersave | performance | conservative
 cpu_governor=ondemand
 cpu_usage_throttle_up=50

 # Limit the max cpu frequency (Mhz) for all cores. | 0=disabled |
 # Useful for lowering temp/power usage on your device.
 cpu_max_frequency=1344

 # Min value 10000 microseconds (10ms)
 cpu_ondemand_sampling_rate=25000

 # sampling rate * down factor / 1000 = Miliseconds (40 = 1000ms when sampling rate is 25000)
 cpu_ondemand_sampling_down_factor=80
*)

  
type governor =
  | Ondemand
  | Powersave
  | Performance
  | Conservative
;;


let _cpu_temp_celsius = "/sys/class/thermal/thermal_zone0/temp" ;;  
let _cpu_available_freqs_kHz cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_available_frequencies" cpu ;;
let _cpu_scaling_max_freq_kHz cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_max_freq" cpu ;;
let _cpu_governor cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_governor" cpu ;;

let read_cpu_temp_celsius () =
  let temp = _cpu_temp_celsius |> read_file Float.of_string in
  temp.(0)
;;
let read_cpu_available_freqs_kHz cpu =
  let freqs = cpu |> _cpu_available_freqs_kHz |> read_file split in
  freqs.(0) |> Array.of_list |> Array.map ~f:Float.of_string
;;
let read_cpu_scaling_max_freq_kHz cpu = 
  let temp = cpu |> _cpu_scaling_max_freq_kHz |> read_file Float.of_string in
  temp.(0)
;;
let read_cpu_governor cpu =
  let temp = cpu |> _cpu_governor |> read_file (fun x -> x) in
  temp.(0)
;;
    
let write_cpu_scaling_max_freq_kHz cpu freq_kHz =
  write_file (_cpu_scaling_max_freq_kHz cpu) (fun outc -> fprintf outc "%f\n" freq_kHz)
;;
let write_cpus_scaling_max_freq_kHz cpus freq_kHz =
  cpus
  |> Array.iter ~f:(fun cpu ->
                     write_file (_cpu_scaling_max_freq_kHz cpu) (fun outc -> fprintf outc "%f\n" freq_kHz))
;;
let write_cpu_governor cpu gov =
  write_file (_cpu_governor cpu) (fun outc -> fprintf outc "%s\n" gov)
;;
let write_cpus_govenor cpus gov =
  cpus
  |> Array.iter ~f:(fun cpu ->
                     write_file (_cpu_governor cpu) (fun outc -> fprintf outc "%s\n" gov))
;;
  

(* Pine64 specific PID settings *)

  

let cpus = [| 0; 1; 2; 3; |] ;;   

let available_freqs_kHz = read_cpu_available_freqs_kHz 0 ;;
let desired_freqs_range_kHz = [| available_freqs_kHz.(5); available_freqs_kHz.(9); available_freqs_kHz.(11) |] ;;

choose_from_ascending available_freqs_kHz (available_freqs_kHz.(5) -. 7.)
|> print_float;;
  
let desired_temp_C = [| 35.; 45.; 60.; |] ;;  

let ratio_freq_temp = desired_freqs_range_kHz.(1) /. desired_temp_C.(1) ;;
  
type state =  {
    _P : float ; _I : float ; _D : float ;
    time_sec : float ; dt_sec : float;
    last_err : float ; cum_err : float ; 
    corr : float; ratio_x_y : float ;
    x : float ; y : float
  }             

                 
let state_0 = {
    _P = 0.; _I = 0.; _D = 0.;
    time_sec = (time()); dt_sec = 0.;
    last_err = 0.; cum_err = 0.; 
    corr = 0.; ratio_x_y = ratio_freq_temp; 
    x = desired_freqs_range_kHz.(1); y = desired_temp_C.(1); }
;;



let dampen corr =
(*
  let (a,b) = (min_corr, max_corr) in
  match a, b with
  | some a, some b -> if corr < a then a
                      else if corr > b then b
                      else corr
  | some a, none -> if corr < a then a else corr
  | none,sSome b -> if corr > b then b else corr                                              
  | _ -> corr
 *)
  corr
;;

  
(* PID control *)
                            
  
let correction state (x: float) (y: float) =
  (* let corr = ref 0. in *)
  
  let t_sec = (time ()) in
  let dt_sec = t_sec -. state.time_sec in
  let error = (state.y -. y) *. state.ratio_x_y in
                      
  (* P correction *)
  let _P = state._P *. error *. dt_sec in

  (* I Correction *)
  let _I = state._I *. state.cum_err *. dt_sec in

  (* D Correction *)
  let slope = (error -. state.last_err) /. dt_sec in
  let _D = state._D *. slope in


  (*let last_err = error in*)
  let cum_err = state.cum_err +. error in
  
  (* dampening the correction value *)
  let corr = dampen (_P +. _I +. _D) in
  {
    _P = _P; _I = _I; _D = _D;
    time_sec = t_sec; dt_sec = dt_sec;
    last_err = error; cum_err = cum_err; 
    corr = corr; ratio_x_y = state.ratio_x_y; 
    x = x +. corr; y = y;
  }
;;


let measure_x () = read_cpu_scaling_max_freq_kHz 0 ;;

let measure_y () = read_cpu_temp_celsius () ;;

let apply (state: state) : state =
  write_cpus_govenor cpus "ondemand";
  write_cpus_scaling_max_freq_kHz cpus state.x;
  state
;;
                     
let control_cpu_temp state =
  try
    (sprintf "governor = %s; temperature = %f C; freq_max = %f kHz" (read_cpu_governor 0) state.y state.x) |> print_endline;
    correction state (measure_x ()) (measure_y ())
    |> apply;
  with _ -> state    
;;
  
let repeat_until_SIGINT delay state f =
  ignore(signal sigint (Signal_handle (fun _ -> raise Break)));
  let rec repeat delay state f =
    sleep delay;
    repeat delay (f state) f in
  try
    print_endline "enter ";
    repeat delay state f;
  with Break ->
    print_endline "\nexit on Break"
;;

  
repeat_until_SIGINT 2 state_0 control_cpu_temp;;
