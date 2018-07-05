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

let bigrapher_version () =
  let channel, buffer = capture_output "bigrapher -V" in
  let _ = Unix.close_process_in channel in
  let contents = Buffer.contents buffer in
  let length = String.length contents in
  String.sub contents 0 (length - 1)
