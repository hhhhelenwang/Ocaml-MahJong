
let play_game () =
  let init = Game.init_state () in
  let state = ref (Game.make_game init) in 
  while Game.is_in_game !state do begin
    state := Game.next_state !state;
  end done

let main () = 
  ANSITerminal.(print_string [cyan] 
                  "\n\nWelcome to the Text-based Richii Mahjong!\n");
  print_endline "Please enter 'start' to start the game.";
  print_string ">>";
  match read_line () with
  | "start" -> play_game ()
  | command -> begin
      let message = "You can't start game with" 
                    ^ command 
                    ^ {|, please restart the game with "make play" and enter start this time.|} in
      ANSITerminal.(print_string [magenta] message);
    end
let () = main ()
