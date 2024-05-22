let () = 
  let e = Harry.from_string "(x*(2*(1+3)))" in 
  let e1 = "2+2+2+2+2+2" in 
  let e2 = Harry.from_string "x*2*1+3" in 
  let dico_of_e = Harry.createDict [("x",2.)] in
  let x = Harry.eval dico_of_e e in 
  print_endline "----------------------";
  print_endline (Harry.autoParenthesis e1);
  print_float x ; 
  print_newline (); 
  print_endline (Harry.to_string e) ;