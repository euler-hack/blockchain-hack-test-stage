exception ErrorMsg string;

datatype 'a optionErr = SOME 'a | RET 'a 'a | NONE string; 

(* OptionErr -> ('a option) *)
fun get x = 
    case x of 
    SOME x => Some x
    | RET _ _ => None
    | NONE msg => (print msg; None);
(* OptionErr -> (string option) *)
fun get_err x =
    case x of 
    NONE x => Some x
    | RET _ _ => None
    | SOME _ => None;
(* OptionErr -> ('a option) *)
fun get_ret_state x = 
    case x of
    RET a _ => Some a 
    | NONE _ => None
    | SOME _ => None;
(* OptionErr -> ('a option) *)
fun get_ret_val x = 
    case x of
    RET _ a => Some a 
    | NONE _ => None
    | SOME _ => None;