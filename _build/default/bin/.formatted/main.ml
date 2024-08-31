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

    let table = Array.make_matrix columns rows 0 in

    let _draw_tile i j size color =
        if i < 0 || i >= row_size || j < 0 || j >= column_size then ()
        else draw_rectangle (i * size) (j * size) size size color
    in

    let _print_table table =
        Array.iter
          (fun row ->
            Array.iter (fun e -> Printf.printf "%d " e) row;
            Printf.printf "\n";
            flush stdout)
          table
    in

    let draw_table table =
        Array.iter
          (fun row ->
            Array.iter (fun e -> ()) row;
            Printf.printf "\n";
            flush stdout)
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

    draw_table table;

    while not (window_should_close ()) do
      begin_drawing ();
      draw_circle 60 60 50. Color.red;

      draw_column 1 column_size;
      draw_row 1 row_size;

      end_drawing ()
    done;
    ()
