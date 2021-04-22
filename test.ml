let fails = ref 0

let test_true f msg =
  match f () with
  | false,r1,r2 -> 
    incr fails;
    msg r1 r2
  | _ -> ()

let repeat test_fun n f msg =
  for i = 1 to n do
    test_fun f (Printf.printf msg i)
  done

let repeat_true n f msg = repeat test_true n f msg

let report name =
  if (!fails > 0) then
    begin
      Printf.printf "tests %s cause %i errors\n" name !fails;
      fails := 0
    end
  else
    Printf.printf "All tests %s are ok!\n" name

module Test (Data: sig include Interface.EQ include Interface.IO_OBJECT with type t := t end) =
struct
  let test f arg res msg =
    let result = f arg in
    if not (Data.equal result res) then
      begin
        incr fails;
        Printf.printf msg (Data.to_string result) (Data.to_string res)
      end
  let simple_test f res msg = test f () res msg
end