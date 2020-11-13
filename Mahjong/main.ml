let play_game () =
  failwith "unimplemented"

let main () = 
  ANSITerminal.(print_string [cyan] 
                  "\n\nWelcome to the Text-base Richii Mahjong!\n");
  print_endline "Please enter 'start' to start the game.";
  print_string ">>";
  match read_line () with
  | command -> play_game ()

let () = main ()