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
  Unix.close_process_in channel, Buffer.contents output_buffer

let bigrapher_version () =
  let _, output = capture_output "bigrapher -V" in
  let length = String.length output in
  String.sub output 0 (length - 1)
