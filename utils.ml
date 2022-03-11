let last_element a =
    let rec aux b=
        match b with 
            | h:: [] -> h
            | h::tl -> aux tl
            | _ -> raise (Failure "last_element")
    in
    aux a
