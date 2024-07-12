open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different kinds
     of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let empty_omok = Game.empty Game.Game_kind.Omok

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;


  let create_empty_list game_kind  = 
  let length = Game.Game_kind.board_length game_kind in 
  let positions = List.join (List.init length ~f:( fun row -> 
    List.init length ~f:(fun column -> 
      Game.Position.{row = row ; column = column}
      ))) in 
      positions
;;


  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;
(*
  let test_case = Game.t_of_sexp (Sexp.of_string 
    "((game_kind Tic_tac_toe)
     (board
      ((((row 0) (column 0)) X) (((row 0) (column 2)) X)
       (((row 1) (column 0)) O) (((row 1) (column 1)) X)
       (((row 2) (column 0)) O) (((row 2) (column 2)) O))))") in *)

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;
  
  let illegal =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 5 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    let kind = game.game_kind in
    let board = game.board in
    let y_length = (Game.Game_kind.board_length kind)*2 - 1 in 
    let x_length = (Game.Game_kind.board_length kind - 1)*3 + Game.Game_kind.board_length kind in
    let list = List.join ( List.init y_length ~f:(fun y ->
      List.init x_length ~f:(fun x ->
        if (equal (y%2) 1 )then("-") else(
          if (equal x 0  || equal (x%4) 0 ) then( 
            match Map.find board Game.Position.{row = y/2; column = x/4 } with 
            | Some pos -> Game.Piece.to_string pos
            | None -> " "
             ) else if(equal (x%2) 0 ) then("|") else(" ")
        )
        )
     )) in
     List.iteri list ~f:(fun i s ->
      if (equal (i%x_length) 0 ) then( Core.print_endline ("") ) else() ;
      Core.printf "%s" s 
     )
  ;; 

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;


  let%expect_test "create empty tic tac toe" =
    List.iter (create_empty_list Game.Game_kind.Tic_tac_toe) ~f:(fun pos -> 
      let s = Game.Position.to_string pos in 
    printf "%s " s);
    [%expect
      {|
      ((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2))
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let board = game.board in 
    let kind = game.game_kind in
    let y_length = (Game.Game_kind.board_length kind) in 
    let x_length = (Game.Game_kind.board_length kind)  in
      let list = List.join ( List.init y_length ~f:(fun y ->
      List.init x_length ~f:(fun x -> 
        let piece = Map.find board Game.Position.{row = y; column = x} in 
          match piece with 
          | None ->  Game.Position.{row = y; column = x}
          | Some _ ->  Game.Position.{row = 3; column = 3}
      ))) in 
      List.filter list ~f:(fun pos -> not (Game.Position.equal Game.Position.{row = 3; column = 3} pos ))
  ;;

  let limited_available_moves radius (game : Game.t) : Game.Position.t list =
    let board = game.board in 
    let kind = game.game_kind in
      let list = List.join (Map.fold board ~init:[] ~f:(fun ~key:pos ~data:_piece -> 
        let og_row = pos.row in 
        let og_column= pos.column in 
       List.append (List.join ( List.init radius ~f:(fun y ->
          (List.init radius ~f:(fun x -> 
              let new_positions = [Game.Position.{row = og_row ; column = x + og_column} ;  Game.Position.{row = y + og_row; column =  - x + og_column} ; Game.Position.{row = og_row; column =  - x + og_column} ; Game.Position.{row = y + og_row; column = og_column}; Game.Position.{row = -  y + og_row; column = og_column}] in 
              List.filter new_positions ~f:( fun new_pos -> 
                not (Map.mem board new_pos ) && 
              Game.Position.in_bounds new_pos ~game_kind:kind 
              )))
        ))
      ))) in
      List.dedup_and_sort list ~compare:(Game.Position.compare)
  ;;


  let%expect_test "moves_non_win" =
    List.iter (limited_available_moves 2 non_win) ~f:(fun pos -> let s = Game.Position.to_string pos in 
    printf "%s " s);
    [%expect
      {|
      ((row 0) (column 1)) ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 1))
      |}];
    return ()
  ;;

  (* Exercise 2 *)


let is_my_piece my_pieces pos board piece = 
  (List.exists my_pieces ~f:(fun this -> Game.Position.equal this pos)) && (String.equal (Game.Piece.to_string (Map.find_exn board pos)) (Game.Piece.to_string piece))
;; (* if this position exists on the board meaning theres a piece there, and that piece is my piece , and its in the direction that I cam from , or its starting a new path , then it can be considered on the path. Lets switch this to work not only for one over but also for leaving a gap or two between depending on win_length *)
let on_path_empty next empty_list = 
 List.exists empty_list ~f:(fun e -> Game.Position.equal next e )
;;

  let rec find_win_with_num direction empty_positions my_pieces game kind num total last_pos piece board win_length = 
    if (equal total win_length) then ((Game.Evaluation.Game_continues, num)) else(
    let _board_length = Game.Game_kind.board_length kind in 
    if (num >= win_length) then ((Game.Evaluation.Game_over { winner = Some piece}, num)) else(
      let next_ = direction last_pos in  
  match next_ with 
  | None -> (Game.Evaluation.Game_continues, 0)
  | Some next -> 
      if(is_my_piece my_pieces next board piece) 
        then(
          find_win_with_num direction empty_positions my_pieces game kind (num+1) (total+1) next piece board win_length
          )
      else if(on_path_empty next empty_positions) then( 
        find_win_with_num direction empty_positions my_pieces game kind num (total + 1) next piece board win_length)
        else(
          if (List.is_empty (available_moves game )) then ((Game.Evaluation.Game_over { winner = None}, num))
                        else (Game.Evaluation.Game_continues,0)
                ) 
            ))
;; 

  let evaluate_with_num (game : Game.t) my_piece =
    let empty_positions = create_empty_list game.game_kind in 
    let board = game.board in 
    let kind = game.game_kind in 
    let length = Game.Game_kind.board_length kind in 
    let win_length = Game.Game_kind.win_length kind in 
    let pos_list = Set.to_list (Map.key_set board) in 
    let my_pieces = List.filter pos_list ~f:(fun pos -> 
      let piece = Map.find_exn board pos in 
      (String.equal (Game.Piece.to_string my_piece) (Game.Piece.to_string piece))
      ) in 
    let empty = List.filter empty_positions ~f:(fun pos -> not (Map.mem board pos) )in
    let is_illegal = List.fold ~init:false pos_list ~f:(fun is pos -> 
      let row = pos.row in 
      let column= pos.column in 
      if (row >= length || column >= length ) then (true) else( is )
      ) in 
      if (is_illegal) then ((Game.Evaluation.Illegal_move,[0])) else(

        let paths  = List.concat_map my_pieces ~f:(fun pos -> 
        let (_, right_num) = find_win_with_num Game.Position.right empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_, down_num) = find_win_with_num Game.Position.down empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_, left_num) = find_win_with_num Game.Position.left empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_, up_num) = find_win_with_num Game.Position.up empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_, up_right_num) = find_win_with_num Game.Position.up_right empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_, up_left_num) = find_win_with_num Game.Position.up_left empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_,down_left_num) = find_win_with_num Game.Position.down_left empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let (_,down_right_num) = find_win_with_num Game.Position.down_right empty my_pieces game kind 1 1 pos my_piece board win_length in 
        let paths  = [right_num; down_num ; down_left_num ; down_right_num ; up_num ; left_num ; up_right_num ; up_left_num ] in 
          let benchmark = match kind with 
          | Tic_tac_toe -> 1
          | Omok -> 2 in 
          List.filter paths ~f:(fun num -> num >= benchmark)
        ) in 
            if ( List.exists paths ~f:(fun path -> equal path win_length)) then (Game.Evaluation.Game_over { winner = Some my_piece }, paths) else(
                (Game.Evaluation.Game_continues , paths)
            )
        )  
  ;;

  let%expect_test "evaluate_non_win" =
  let (eval,_) = (evaluate_with_num non_win Game.Piece.X) in 
  print_s (Game.Evaluation.sexp_of_t  eval);
  [%expect
    {|
    Game_continues 
    |}];
  return ()
;;

let%expect_test "evaluate_win_for_x" =
  let (eval,_) = (evaluate_with_num win_for_x Game.Piece.X) in 
  print_s (Game.Evaluation.sexp_of_t  eval);
  [%expect
    {|
    (Game_over (winner (X)))
    |}];
  return ()
;;

let%expect_test "evaluate_illegal" =
let (eval,_) = (evaluate_with_num illegal Game.Piece.X) in 
print_s (Game.Evaluation.sexp_of_t  eval);
  [%expect
    {|
    Illegal_move
    |}];
  return ()
;;

  (* Exercise 3 *)

  let available_adjacent_moves game piece =
    let all_moves = available_moves game in 
    let board = game.board in 
    let my_pieces = Map.filter board ~f:(fun p -> 
      (String.equal (Game.Piece.to_string (p)) (Game.Piece.to_string piece))
      ) in 
   Map.fold my_pieces ~init:[] ~f:(fun ~key:pos ~data:_piece -> 
      let neighbors = List.filter_opt [Game.Position.left pos ; Game.Position.right pos ;Game.Position.down pos ;Game.Position.up pos ;Game.Position.down_left pos ;Game.Position.down_right pos  ] in  
      List.append (List.filter neighbors ~f:(fun neighbor -> 
         (List.exists all_moves ~f:(fun p ->  
          Game.Position.equal p neighbor
          ) 
      ) )))
  ;; (* ask TA if this works the way I think it does *)


  let winning_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    match evaluate_with_num game me with 
    | (Game_continues, _) -> 
      let moves = available_adjacent_moves game me in 
      if (List.is_empty moves ) then ([]) else (
          List.fold moves ~init:[] ~f:(fun wins pos ->
            let new_game = place_piece game ~piece:me ~position:pos in 
            let new_outcome = evaluate_with_num new_game me in 
            match new_outcome with 
            | (Game_over {winner = Some piece},_) when Game.Piece.equal me piece -> (List.append wins [pos]) 
            | _ -> wins
            )
      )
    | _ -> []
  ;; (* maybe check the paths and then if therees a num thats 1 less then you have a winning move? but determining what that move is is a bit more tricky*)
    let blocking_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
      match evaluate_with_num game me with 
      | (Game_continues, _) -> 
        let moves = available_adjacent_moves game me in 
        if (List.is_empty moves ) then ([]) else (
            List.fold moves ~init:[] ~f:(fun wins pos ->
              let new_game = place_piece game ~piece:me ~position:pos in 
              let new_outcome = evaluate_with_num new_game me in 
              match new_outcome with 
              | (Game_over {winner = Some piece},_) when Game.Piece.equal me piece -> (List.append wins [pos]) 
              | (Game_continues, new_num) when (List.mem new_num 4 ~equal:(equal)) -> (List.append wins [pos]) 
              | _ -> wins
              )
        )
      | _ -> []
    ;;


  let%expect_test "winning_moves_non_win" =
  let win = winning_moves ~me:Game.Piece.X non_win in 
  List.iter (win) ~f:(fun pos -> let s = Game.Position.to_string pos in 
  printf "%s " s);
  [%expect
    {|
    ((row 1) (column 1))
    |}];
  return ()
;;

  (* Exercise 4 *) 
  let losing_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list = 
    let not_me = Game.Piece.flip me in 
    match evaluate_with_num game me with 
    | (Game_continues,_) -> 
      let moves = available_moves game in 
      if (List.is_empty moves ) then ([]) else (
          List.fold moves ~init:[] ~f:(fun losses pos ->
            let new_game = place_piece game ~piece:me ~position:pos in 
            let new_outcome = evaluate_with_num new_game me in 
            match new_outcome with 
            | (Game_continues,_) -> 
              let other_wins = winning_moves ~me:not_me new_game in 
              if (not (List.is_empty other_wins)) then((List.append losses [pos]) ) else(losses)
            | _ -> losses
          )
      )
    | _ -> []
  ;;

  let%expect_test "losing_moves_non_win" =
  let lose = losing_moves ~me:Game.Piece.O non_win in 
  List.iter (lose) ~f:(fun pos -> let s = Game.Position.to_string pos in 
  printf "%s " s);
  [%expect
    {|
     ((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2) (column 1)) 
    |}];
  return ()
;;

let available_moves_that_do_not_immediately_lose ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list = 
  let moves = available_moves game in 
  let losing_moves = losing_moves ~me game in 
  List.filter moves ~f:(fun move -> not (List.exists losing_moves ~f:(fun losing_move -> Game.Position.equal losing_move move )))
;;


let score current_player (maxplayer : Game.Piece.t) (game : Game.t) depth = (* its maxplayers turn ? i think*)
let _win_length = Game.Game_kind.win_length game.game_kind in 
let kind = game.game_kind in 
  let (eval, num) = evaluate_with_num game current_player in 
  match eval with 
  | Illegal_move -> 0
  | Game_over {winner = Some piece} when Game.Piece.equal maxplayer piece -> (Int.max_value - (5-depth)) 
  | Game_over {winner = Some piece} when not(Game.Piece.equal maxplayer piece) -> (Int.min_value + (5-depth))
  | Game_over _ -> 0
  | Game_continues -> 
    let multiplier = match (Game.Piece.equal maxplayer current_player) with 
    | true -> 1
    |false -> -1 
  in  
  let my_value = match kind with 
  | Tic_tac_toe -> 
    let vals = (List.map num ~f:(fun length -> 
      match length with 
      | 1 -> 10
      | 2 -> 1000
      | _ -> 0)) in 
    List.fold ~init:0 vals ~f:(fun curr vals -> 
      (vals + curr )
    )
  | Omok ->
    let vals = (List.map num ~f:(fun length -> 
      match length with 
      | 2 -> 5
      | 3 -> 100
      | 4 -> 10000
      | _ -> 0 ))in 
      List.fold ~init:0 vals ~f:(fun curr vals -> 
        (vals + curr )
      ) in 
    (multiplier*my_value) (*+ opp_value *)
;;

type blah = {mutable count : int}
let (count : blah) = { count =  0}
let rec minimax game depth (current_player: Game.Piece.t) max_player = 
  count.count <- count.count + 1;
  (*Core.print_s [%message (count.count : int)];*)
  let next_player = Game.Piece.flip current_player in
  let eval = evaluate_with_num game current_player in 
  let is_terminal = 
  match eval with 
  | (Game_over _, _ ) -> true 
  | (Illegal_move, _ ) -> true 
  | _ -> false
  in 
  if (equal depth 0 || is_terminal) then (
   (score current_player max_player game depth )
  ) else (
      if(Game.Piece.equal current_player max_player) then( 
        let moves = limited_available_moves 6 game in 
        let value = Int.min_value in 
        List.fold moves ~init:0 ~f:(fun _val move -> 
          let new_game = place_piece game ~piece:current_player ~position:move in
          Int.max value (minimax new_game (depth - 1) next_player max_player )
          )
      ) else (
        let value = Int.max_value in 
        let moves = limited_available_moves 6 game in 
        List.fold moves ~init:0 ~f:(fun _val move -> 
          let new_game = place_piece game ~piece:current_player ~position:move in
          Int.min value (minimax new_game (depth - 1) next_player max_player )
          )
      )
  )
;; (* look at all available moves and find the "new game" for each one. compare minimaxes and then choose the best  *)


let they_played_corner corners game = 
  let available_moves = available_moves game in 
  let num = List.fold available_moves ~init:0 ~f:(fun n move -> 
    if (List.exists corners ~f:(fun corner -> Game.Position.equal move corner)) then (n+1) else (n)
    ) in 

    if (equal num 4) then false else true
;;


let choose_move (game:Game.t) player = 
  let next_player = Game.Piece.flip player in 
  let center = Game.Position.{row = 1 ; column = 1} in
  let corners = [ Game.Position.{row = 0 ; column = 0} ; Game.Position.{row = 0 ; column = 2} ; Game.Position.{row = 2 ; column = 0} ; Game.Position.{row = 2 ; column = 2} ] in
  let board = game.board in 
  let available_moves = limited_available_moves 3 game in 
  if (Map.is_empty board ) then (Game.Position.{row = 7 ; column = 8 }) else( (* first move is the corner *)
  if ((Game.Game_kind.equal Game.Game_kind.Tic_tac_toe game.game_kind)&& (equal (List.length available_moves) 8) && (they_played_corner corners game)) then (center) else (* if they play a corner, second move is the center *)
  let my_winning_moves = winning_moves ~me:player game in 
  let blocking_moves = blocking_moves ~me:next_player game in 
  if (not (List.is_empty my_winning_moves)) then (List.hd_exn my_winning_moves) else if (not (List.is_empty blocking_moves) ) then (List.hd_exn blocking_moves) else (
  let (ideal_move, _ ) = List.fold available_moves ~init:(Game.Position.{row = 0 ; column = 0 }, Int.min_value) ~f:(fun prev move -> 
    let new_game = place_piece game ~piece:player ~position:move in 
    let value = minimax new_game 1 next_player player in 
    let (_, prev_val) = prev in 
    if (value > prev_val) then ( (move,value)) else(prev)
    ) in 
    ideal_move
  )
  )
;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let (evaluation,_) = evaluate_with_num win_for_x Game.Piece.X in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let (evaluation,_) = evaluate_with_num non_win Game.Piece.X in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all |> List.map ~f:Game.Piece.to_string |> String.concat ~sep:", ")
        )
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;


  let exercise_five =
    Command.async
      ~summary:"Exercise 5: What moves do not immediately lose?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let good_moves = available_moves_that_do_not_immediately_lose ~me:piece non_win in
         print_s [%sexp (good_moves : Game.Position.t list)];
         return ())
       ;;

       let exercise_six =
        Command.async
          ~summary:"Exercise 6: Omok against itself"
          (let%map_open.Command () = return ()
           and piece = piece_flag in
           fun () ->
            
            let list_moves = List.init 228 ~f:(fun i -> i ) in 
            let _new_game =  List.fold list_moves ~init:(empty_omok,piece) ~f:(fun (prev_game,prev_piece) _i -> 
              Core.print_s [%message "BOARD: "];
               let move = choose_move prev_game prev_piece in
               let new_game =place_piece prev_game ~piece:prev_piece ~position:move in 
               let state = (new_game, Game.Piece.flip prev_piece) in 
               Core.print_s [%message (move : Game.Position.t)];
              print_game new_game ; 
              state
             ) in 
             return ())
           ;;
    

let command =
  Command.group
    ~summary:"Exercises"
    [ "one"  , exercise_one
    ; "two"  , exercise_two
    ; "three", exercise_three
    ; "four" , exercise_four
    ; "five" , exercise_five
    ; "six" , exercise_six
    ]
;;
end 

let handle (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Received query" (query : Rpcs.Take_turn.Query.t)];
  let game = query.game in 
  let piece = query.you_play in
  let response = { Rpcs.Take_turn.Response.piece = piece ; Rpcs.Take_turn.Response.position = (Exercises.choose_move game piece) } in
  return response
;;


let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle incoming
          queries for [Take_turn] and [Game_over]. We should also connect to the
          controller and send a [Start_game] to initiate the game. *)
         
             let%bind server =
               Rpc.Connection.serve
                 ~implementations
                 ~initial_connection_state:(fun _client_identity _client_addr ->
                   (* This constructs the "client" values which are passed to the
                      implementation function above. We're just using unit for now. *)
                   ())
                 ~where_to_listen:(Tcp.Where_to_listen.of_port port)
                 ()
             in
             Tcp.Server.close_finished server )
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
