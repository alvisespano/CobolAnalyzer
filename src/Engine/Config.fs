(*
 * COBOL Type Inference
 * Config.fs: static configuration
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Config

open System
open Prelude

let exe_name = "CobolInference"
let mutable print_typed_source = false

module Parsing =

    let mutable newline_is_separator = true


module Typing =

    let mutable tolerant = true

    module Layout =
        let float32_max_integral_digits = 7us
        let float64_max_integral_digits = 15us
        let float32_max_fractional_digits = 7us
        let float64_max_fractional_digits = 15us


module Trans =

    let fresh_var_numformat_size = 8us


module Log =   

    let mutable filename = "log.txt"

    #if DEBUG
    let mutable debug_threshold = Min
    let mutable msg_threshold = Min
    let mutable hint_threshold = Min
    let mutable warn_threshold = Min
    #else
    let mutable debug_threshold = Unmaskerable
    let mutable msg_threshold = Normal
    let mutable hint_threshold = Normal
    let mutable warn_threshold = Normal
    #endif

    let set_all_thresholds p =
        debug_threshold <- p;
        msg_threshold <- p;
        hint_threshold <- p;
        warn_threshold <- p;

    let mutable show_datetime = false
    let mutable show_priority = true
    let mutable show_urgency = true
    let mutable shade_urgency = true

    let multiline_tab_width = 3

    module Colors =
        let datetime = ConsoleColor.DarkMagenta
        let priority = ConsoleColor.Blue
        let urgency = ConsoleColor.White
        let msg = ConsoleColor.Gray
        let debug = ConsoleColor.Cyan
        let hint = ConsoleColor.Green
        let warn = ConsoleColor.Yellow
        let error = ConsoleColor.Red