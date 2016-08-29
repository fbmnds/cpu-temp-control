open Sys;;
open Unix;;
open Format;;
open Core.Std;; (* M-x merlin-use *)
open Str;;
  
exception Break;;

(*
  #Cpu Governor | ondemand | powersave | performance | conservative
  cpu_governor=ondemand
  cpu_usage_throttle_up=50

 #Limit the max cpu frequency (Mhz) for all cores. | 0=disabled | Useful for lowering temp/power usage on your device.
 cpu_max_frequency=1344

 #Min value 10000 microseconds (10ms)
 cpu_ondemand_sampling_rate=25000

 #sampling rate * down factor / 1000 = Miliseconds (40 = 1000ms when sampling rate is 25000)
 cpu_ondemand_sampling_down_factor=80
*)

type governor =
  | Ondemand
  | Powersave
  | Performance
  | Conservative
;;


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


 let split = Str.split (Str.regexp_string " ") ;;  
  
  
(* CPU interfaces for Pine64 on DietPi *)

let _cpu_temp_celsius = "/sys/class/thermal/thermal_zone0/temp" ;;  
let _cpu_available_freqs_kHz cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_available_frequencies" cpu ;;
let _cpu_scaling_max_freq_kHz cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_max_freq" cpu ;;
let _cpu_covernor cpu = sprintf "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_governor" cpu ;;

let read_cpu_temp_celsius () =
  let temp = _cpu_temp_celsius |> read_file Int.of_string in
  temp.(0)
;;
let read_cpu_available_freqs_kHz cpu =
  let freqs = cpu |> _cpu_available_freqs_kHz |> read_file split in
  freqs.(0) |> Array.of_list |> Array.map ~f:Int.of_string
;;
let read_cpu_scaling_max_freq_kHz cpu = 
  let temp = cpu |> _cpu_scaling_max_freq_kHz |> read_file Int.of_string in
  temp.(0)
;;
let read_cpu_covernor cpu =
  let temp = cpu |> _cpu_covernor |> read_file (fun x -> x) in
  temp.(0)
;;
    
let write_cpu_scaling_max_freq_kHz cpu freq = () ;;
let write_cpu_covernor cpu gov = () ;;
  
  
type state =  {
    _P : float ; _I : float ; _D : float ;
    last_err : float ; cum_err : float ; time: float ;
    corr : float; ratio_x_y : float ;
    x : float ; y : float
  }             

                 
let state_0 = {
    _P = 0.; _I = 0.; _D = 0.;
    last_err = 0.; cum_err = 0.; time = (time());
    corr = 0.; ratio_x_y = (1032. /. 30.); 
    x = 1032.; y = 30.; }
;;

                             
let dampen (min_corr: float option) (max_corr: float option) corr =
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

  
let correction state (x: float) (y: float) =
  (* let corr = ref 0. in *)
  
  let t = (time ()) in
  let dt = t -. state.time in
  let error = (state.y -. y) *. state.ratio_x_y in
                      
  (* P correction *)
  let _P = state._P *. error *. dt in

  (* I Correction *)
  let _I = state._I *. state.cum_err *. dt in

  (* D Correction *)
  let slope = (error -. state.last_err) /. dt in
  let _D = state._D *. slope in


  (*let last_err = error in*)
  let cum_err = state.cum_err +. error in
  
  (* dampening the correction value *)
  let corr = dampen (Some 500.) (Some 1100.) (_P +. _I +. _D) in
  {
    _P = _P; _I = _I; _D = _D;
    last_err = error; cum_err = cum_err; time = t;
    corr = corr; ratio_x_y = state.ratio_x_y; 
    x = x +. corr; y = y;
  }
;;


let measure_x () = 1050.;;
 
let control_cpu_temp state =
  (sprintf "temperature = %f" state.y) |> print_endline;
  correction state (measure_x ()) (Float.of_int (read_cpu_temp_celsius ()));;
  
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
