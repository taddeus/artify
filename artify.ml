open CamomileLibrary

let input_all ic =
  let bufsize = 512 in
  let buf = Bytes.create bufsize in
  let rec loop cur =
    match input ic buf 0 bufsize with
    | 0 -> close_in ic; cur
    | nread -> loop (cur ^ Bytes.sub_string buf 0 nread)
  in
  loop ""

let pad_right s =
  let rec repeat s n = if n < 1 then "" else s ^ (repeat s (n - 1)) in
  let lines = Str.split (Str.regexp " *\n") s in
  let cmp_length max line =
    let l = String.length line in
    if l > max then l else max
  in
  let w = List.fold_left cmp_length 0 lines in
  let pad buf line = buf ^ line ^ repeat " " (w - String.length line + 1) ^ "\n" in
  List.fold_left pad "" lines

let artify replace_pat s =
  let w = try String.index s '\n' + 1 with Not_found -> String.length s in
  let h = String.length s / w in
  let char_at x y =
    if x < 0 || y < 0 || x >= w || y >= h
      then ' '
      else String.get s (y * w + x)
  in
  let make_pat i =
    let x = i mod w in
    let y = i / w in
    let o ox oy = char_at (x + ox) (y + oy) in
    let m1 = -1 in
    (o m1 m1, o 0 m1, o 1 m1,
     o m1  0, o 0  0, o 1  0,
     o m1  1, o 0  1, o 1  1)
  in
  UTF8.init (String.length s) (fun i -> replace_pat (make_pat i))

let u s = UTF8.get s 0

let replace_pat_safe = function
  |     _    , ('|'|'+'),     _    ,
    ('-'|'+'),    '+'   , ('-'|'+'),
        _    , ('|'|'+'),     _      -> u "┼"
  |     _    ,     _    ,     _    ,
    ('-'|'+'),    '+'   , ('-'|'+'),
        _    , ('|'|'+'),     _      -> u "┬"
  |     _    , ('|'|'+'),     _    ,
    ('-'|'+'),    '+'   , ('-'|'+'),
        _    ,     _    ,     _      -> u "┴"
  |     _    , ('|'|'+'),     _    ,
        _    ,    '+'   , ('-'|'+'),
        _    , ('|'|'+'),     _      -> u "├"
  |     _    , ('|'|'+'),     _    ,
    ('-'|'+'),    '+'   ,     _    ,
        _    , ('|'|'+'),     _      -> u "┤"
  |     _    ,     _    ,     _    ,
        _    ,    '+'   , ('-'|'+'),
        _    , ('|'|'+'),     _      -> u "┌"
  |     _    , ('|'|'+'),     _    ,
        _    ,    '+'   , ('-'|'+'),
        _    ,     _    ,     _      -> u "└"
  |     _    ,     _    ,     _    ,
    ('-'|'+'),    '+'   ,     _    ,
        _    , ('|'|'+'),     _      -> u "┐"
  |     _    , ('|'|'+'),     _    ,
    ('-'|'+'),    '+'   ,     _    ,
        _    ,     _    ,     _      -> u "┘"
  |     _    ,     _    ,     _    ,
        _    ,    '-'   , ('-'|'+'),
        _    ,     _    ,     _
  |     _    ,     _    ,     _    ,
    ('-'|'+'),    '-'   ,     _    ,
        _    ,     _    ,     _      -> u "─"
  |     _    ,     _    ,     _    ,
        _    ,    '|'   ,     _    ,
        _    , ('|'|'+'),     _
  |     _    , ('|'|'+'),     _    ,
        _    ,    '|'   ,     _    ,
        _    ,     _    ,     _      -> u "│"
  |     _    , ('|'|'+'),     _    ,
       '='   ,    '+'   ,    '='   ,
        _    , ('|'|'+'),     _      -> u "╪"
  |     _    ,     _    ,     _    ,
       '='   ,    '+'   ,    '='   ,
        _    , ('|'|'+'),     _      -> u "╤"
  |     _    , ('|'|'+'),     _    ,
       '='   ,    '+'   ,    '='   ,
        _    ,     _    ,     _      -> u "╧"
  |     _    , ('|'|'+'),     _    ,
        _    ,    '+'   ,    '='   ,
        _    , ('|'|'+'),     _      -> u "╞"
  |     _    , ('|'|'+'),     _    ,
       '='   ,    '+'   ,     _    ,
        _    , ('|'|'+'),     _      -> u "╡"
  |     _    ,     _    ,     _    ,
        _    ,    '+'   ,    '='   ,
        _    , ('|'|'+'),     _      -> u "╒"
  |     _    , ('|'|'+'),     _    ,
        _    ,    '+'   ,    '='   ,
        _    ,     _    ,     _      -> u "╘"
  |     _    ,     _    ,     _    ,
       '='   ,    '+'   ,     _    ,
        _    , ('|'|'+'),     _      -> u "╕"
  |     _    , ('|'|'+'),     _    ,
       '='   ,    '+'   ,     _    ,
        _    ,     _    ,     _      -> u "╛"
  |     _    ,     _    ,     _    ,
        _    ,    '='   , ('='|'+'),
        _    ,     _    ,     _
  |     _    ,     _    ,     _    ,
    ('='|'+'),    '='   ,     _    ,
        _    ,     _    ,     _      -> u "═"
  |     _    ,     _    ,     _    ,
        _    ,     c    ,     _    ,
        _    ,     _    ,     _      -> UChar.of_char c

let replace_pat_unsafe =
  let both_double x y = if x = y then x else '-' in
  let pick_double x s = UTF8.get s (if x = '=' then 1 else 0) in
  let is_dash x = x = '-' || x = '=' in
  let are_dash x y = is_dash x && is_dash y in
  function
  |  _ , ' ',  _ ,
    ' ', '+', ' ',
     _ , ' ',  _   -> u "·"  (* XXX: maybe just leave the '+' *)
  |  _ , ' ',  _ ,
     _ , '+', ' ',
     _ , ' ',  _   -> u "╴"
  |  _ , ' ',  _ ,
    ' ', '+',  _ ,
     _ , ' ',  _   -> u "╶"
  |  _ , ' ',  _ ,
    ' ', '+', ' ',
     _ ,  _ ,  _   -> u "╷"
  |  _ ,  _ ,  _ ,
    ' ', '+', ' ',
     _ , ' ',  _   -> u "╵"
  |  _ , ' '      ,  _ ,
    ' ', ('+'|','),  x ,
     _ ,  _       ,  _   when is_dash x -> pick_double x "┌╒"
  |  _ ,     _    ,  _ ,
    ' ', ('+'|'`'),  x ,
     _ ,    ' '   ,  _   when is_dash x -> pick_double x "└╘"
  |  _ ,    ' '   ,  _ ,
     x , ('+'|'.'), ' ',
     _ ,     _    ,  _   when is_dash x -> pick_double x "┐╕"
  |  _ ,     _    ,  _ ,
     x , ('+'|'\''), ' ',
     _ ,    ' '   ,  _   when is_dash x -> pick_double x "┘╛"
  |  _ ,    ' '   ,  _ ,
     x ,    '+'   ,  y ,
     _ ,     _    ,  _   when are_dash x y -> pick_double (both_double x y) "┬╤"
  |  _ ,     _    ,  _ ,
     x ,    '+'   ,  y ,
     _ ,    ' '   ,  _   when are_dash x y -> pick_double (both_double x y) "┴╧"
  |  _ ,     _    , _ ,
    ' ', ('+'|'|'),  x,
     _ ,     _    ,  _   when is_dash x -> pick_double x "├╞"
  |  _ ,     _    ,  _ ,
     x , ('+'|'|'), ' ',
     _ ,     _    ,  _   when is_dash x -> pick_double x "┤╡"
  |  _ ,     _    ,  _ ,
     x , ('+'|'|'),  y ,
     _ ,     _    ,  _   when are_dash x y -> pick_double (both_double x y) "┼╪"

  |     _    ,     _    ,     _    ,
        _    ,    '-'   , ('-'|'+'),
        _    ,     _    ,     _
  |     _    ,     _    ,     _    ,
    ('-'|'+'),    '-'   ,     _    ,
        _    ,     _    ,     _      -> u "─"

  |     _    ,     _    ,     _    ,
        _    ,    '|'   ,     _    ,
        _    , ('|'|'+'),     _
  |     _    , ('|'|'+'),     _    ,
        _    ,    '|'   ,     _    ,
        _    ,     _    ,     _      -> u "│"

  |       _      ,  _ , _ ,
    ('-'|'='|'+'), '=', _ ,
          _      ,  _ , _
  | _ ,  _ ,       _      ,
    _ , '=', ('-'|'='|'+'),
    _ ,  _ ,       _        -> u "═"

  | _, _, _,
    _, c, _,
    _, _, _  -> UChar.of_char c

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let replace_pat =
    if List.mem "-s" args
      then replace_pat_safe
      else replace_pat_unsafe
  in
  stdin |> input_all |> pad_right |> artify replace_pat |> print_endline
