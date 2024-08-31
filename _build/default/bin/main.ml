open Raylib

let () =
    let screenWidth = 800 in
    let screenHeight = 800 in
    init_window screenHeight screenWidth "test";

    set_target_fps 60;

    let columns = 10 in
    let rows = 10 in

    let column_size = screenWidth / columns in

    let row_size = screenHeight / rows in

    let table = Array.make_matrix columns rows false in
    let table2 = Array.make_matrix columns rows false in

    let draw_tile i j size color =
        if i < 0 || i >= row_size || j < 0 || j >= column_size then ()
        else draw_rectangle (j * size) (i * size) size size color
    in

    let print_table table =
        Array.iter
          (fun row ->
              Array.iter (fun e -> if e then Printf.printf "1";) row;
            Printf.printf "\n";
            flush stdout)
          table
    in

    let draw_table table =
        Array.iteri
          (fun i row ->
            Array.iteri (fun j e -> begin 
                if e then draw_tile i j 80 Color.white
                else draw_tile i j 80 Color.black
            end) row)
          table
    in

    let rec draw_column i size =
        if i * size >= screenWidth then ()
        else (
          draw_line (i * size) 0 (i * size) screenHeight Color.white;
          draw_column (i + 1) size)
    in

    let rec draw_row i size =
        if i * size >= screenHeight then ()
        else (
          draw_line 0 (i * size) screenWidth (i * size) Color.white;
          draw_row (i + 1) size)
    in

    let mCopy table table2 =
        Array.iteri
          (fun i row ->
              Array.iteri (fun j e -> table2.(i).(j) <- e;) row)
          table;
    in
    
    let nuNeibor _ _ _ =
        let dx = [|-1; 0; 1|] in
        Array.iteri (fun _ x -> Array.iteri (fun _ y -> Printf.printf "(%d, %d)\n" x y) dx ) dx;
        (* let a = if table.(i-1).(j-1) then 1 else 0 in *)
        (* let b = if table.(i).(j-1) then 1 else 0 in *)
    in

    nuNeibor 0 0 table;

    let _update_table table table2 =
        Array.iteri
          (fun i row ->
              Array.iteri (fun j _ -> 
                  table2.(i).(j) <- begin

                  end


              ) row)
          table;
    in

    print_table table;

    let inGame = ref false in

    while not (window_should_close ()) do
        if not !inGame then
            if is_mouse_button_pressed Raylib.MouseButton.Left then
                let mpos = Raylib.get_mouse_position() in
                let x = Vector2.x mpos in
                let y = Vector2.y mpos in
                let x = (int_of_float x)/row_size in
                let y = (int_of_float y)/row_size in
                table.(y).(x) <- not table.(y).(x);
                Printf.printf "%d - %d\n" x y;
                flush stdout;
            else ()
        else
            mCopy table table2;
            (* update_table table table2; *)
            mCopy table2 table;
            

        if is_key_pressed Key.A then print_table table;
        begin_drawing ();
        draw_table table;
        draw_column 1 column_size;
        draw_row 1 row_size;
        end_drawing ()
    done;
    ()
