
datatype 'a strcon =
  strcon_nil
| strcon_cons of
  ('a * (unit -> 'a strcon));

type 'a stream = (unit -> 'a strcon);

fun helper(char: char): char stream =
  fn() =>
    if char = #"A" then
      strcon_cons(#"A", helper(#"B"))
    else if char = #"B" then
      strcon_cons(#"B", helper(#"C"))
    else if char = #"C" then
      strcon_cons(#"C", helper(#"D"))
    else if char = #"D" then
      strcon_cons(#"D", helper(#"E"))
    else if char = #"E" then
      strcon_cons(#"E", helper(#"F"))
    else if char = #"F" then
      strcon_cons(#"F", helper(#"G"))
    else if char = #"G" then
      strcon_cons(#"G", helper(#"H"))
    else if char = #"H" then
      strcon_cons(#"H", helper(#"I"))
    else if char = #"I" then
      strcon_cons(#"I", helper(#"J"))
    else if char = #"J" then
      strcon_cons(#"J", helper(#"K"))
    else if char = #"K" then
      strcon_cons(#"K", helper(#"L"))
    else if char = #"L" then
      strcon_cons(#"L", helper(#"M"))
    else if char = #"M" then
      strcon_cons(#"M", helper(#"N"))
    else if char = #"N" then
      strcon_cons(#"N", helper(#"O"))
    else if char = #"O" then
      strcon_cons(#"O", helper(#"P"))
    else if char = #"P" then
      strcon_cons(#"P", helper(#"Q"))
    else if char = #"Q" then
      strcon_cons(#"Q", helper(#"R"))
    else if char = #"R" then
      strcon_cons(#"R", helper(#"S"))
    else if char = #"S" then
      strcon_cons(#"S", helper(#"T"))
    else if char = #"T" then
      strcon_cons(#"T", helper(#"U")
    else if char = #"U" then
      strcon_cons(#"U", helper(#"V"))
    else if char = #"V" then
      strcon_cons(#"V", helper(#"W"))
    else if char = #"W" then
      strcon_cons(#"W", helper(#"X"))
    else if char = #"X" then
      strcon_cons(#"X", helper(#"Y"))
    else if char = #"Y" then
      strcon_cons(#"Y", helper(#"Z"))
    else
      strcon_cons(#"Z", helper(#"A"));

fun alphabeta_cycling_list(): char stream =
  fn() => helper(#"A");

