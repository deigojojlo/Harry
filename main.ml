let () = 
  let e = Harry.from_string "(x*(2*(1+3)))" in 
  let dico_of_e = Harry.createDict [("x",2.)] in
  let x = Harry.eval dico_of_e e in 
  print_float x ; 
  print_newline (); 
  print_endline (Harry.to_string e) ;