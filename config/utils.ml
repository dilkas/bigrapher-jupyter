(* Run a command, capturing its output in a buffer *)
let capture_output command =
  let channel = Unix.open_process_in command in
  let output_buffer = Buffer.create 256 in
  begin
    try
      while true do
        Buffer.add_channel output_buffer channel 1
      done
    with End_of_file -> ()
  end ;
  channel, output_buffer
