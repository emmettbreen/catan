let cyan_print_endline str =
  ANSITerminal.print_string [ ANSITerminal.cyan ] str;
  print_endline ""

let cyan_print = ANSITerminal.print_string [ ANSITerminal.cyan ]

let magenta_print_endline str =
  ANSITerminal.print_string [ ANSITerminal.magenta ] str;
  print_endline ""

let magenta_print = ANSITerminal.print_string [ ANSITerminal.magenta ]

let red_print_endline str =
  ANSITerminal.print_string [ ANSITerminal.red ] str;
  print_endline ""

let red_print = ANSITerminal.print_string [ ANSITerminal.red ]

let green_print_endline str =
  ANSITerminal.print_string [ ANSITerminal.green ] str;
  print_endline ""

let green_print = ANSITerminal.print_string [ ANSITerminal.green ]

let yellow_print_endline str =
  ANSITerminal.print_string [ ANSITerminal.yellow ] str;
  print_endline ""

let yellow_print = ANSITerminal.print_string [ ANSITerminal.yellow ]

let white_print_endline = print_endline

let white_print = print_string

let style_printing styles str = ANSITerminal.print_string styles str

let get_color_from_hash str offset =
  let index = (Hashtbl.hash str mod 4) + offset in
  match index with
  | 1 -> ANSITerminal.yellow
  | 2 -> ANSITerminal.green
  | 3 -> ANSITerminal.red
  | _ -> ANSITerminal.blue

let get_color_from_index index =
  match index with
  | 0 -> ANSITerminal.yellow
  | 1 -> ANSITerminal.green
  | 2 -> ANSITerminal.red
  | _ -> ANSITerminal.blue

let color_print style str = ANSITerminal.print_string [ style ] str
