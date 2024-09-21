(*
f1c1 f1c2 f2c3 f2c4
f2c1 f2c2 f2c3 f2c4
f3c1 f3c2 f3c3 f3c4
*)
let realizarMovidad = fun f1 c1 f2 c2 -> (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                        let (tablero, nivel, repartir, puntos) = !estado in
                                        estado := ((setElem tablero f1 c1 0), nivel, repartir, puntos);

                                        let (tablero, nivel, repartir, puntos) = !estado in
                                        estado := ((setElem tablero f2 c2 0), nivel, repartir, puntos);
                                        
                                        (*Comprueba el estado del tablero*)
                                        let (tablero, nivel, repartir, puntos) = !estado in
                                        estado := ((comprobarEstadoTablero tablero), nivel, repartir, puntos);;

(*FUNCIONES PARA COMPROBAR EL PRIMER NIVEL*)
(*Comprueba si el numero de f1c1 y el de f2c2 son iguales o si sumados dan 10*)  
let comprobarPrimerNivel = fun f1 c1 f2 c2 -> let (tablero, nivel, repartir, puntos) = !estado in
                                              if (f1 = f2 && c1 = c2) then false else
                                              if ((getElem tablero f1 c1) = 0 || (getElem tablero f2 c2) = 0) then false else
                                              if ((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) 
                                              then(
                                                if ((abs (f1-f2) <=1) && (abs (c1-c2) <=1)) then(
                                                    (*Llama la funcion de sumar puntos*)
                                                    sumarPuntos 1;

                                                    (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                    (realizarMovidad f1 c1 f2 c2);

                                                    true
                                                ) else if (f1 = f2) then(
                                                    let (tablero, nivel, repartir, puntos) = !estado in
                                                    if (((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) & (comprobarHorizontal tablero f1 c1 f2 c2 0)) then(
                                                        (*Llama la funcion de sumar puntos*)
                                                        sumarPuntos 4;

                                                        (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                        (realizarMovidad f1 c1 f2 c2);

                                                        true
                                                    ) else false
                                                ) else if (c1 = c2) then(
                                                    let (tablero, nivel, repartir, puntos) = !estado in
                                                    if (((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) & (comprobarVertical tablero f1 c1 f2 c2 0)) then(
                                                        (*Llama la funcion de sumar puntos*)
                                                        sumarPuntos 4;

                                                        (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                        (realizarMovidad f1 c1 f2 c2);

                                                        true
                                                    ) else false
                                                ) else (
                                                    let (tablero, nivel, repartir, puntos) = !estado in
                                                    if (((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) & (comprobarDiagonal tablero f1 c1 f2 c2 0 (lenght tablero) 9)) then(
                                                        (*Llama la funcion de sumar puntos*)
                                                        sumarPuntos 4;

                                                        (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                        (realizarMovidad f1 c1 f2 c2);

                                                        true
                                                    ) else false
                                                )
                                              )
                                              else false;;




let rec comprobarHorizontal = fun tablero f1 c1 f2 c2 n -> if (c1 = c2) then true else
                                                        if (c1 < c2) then(
                                                            if (n = 0) then( 
                                                                comprobarHorizontal tablero f1 (c1+1) f2 c2 1
                                                            )else if ((getElem tablero f1 c1) = 0) then( 
                                                                comprobarHorizontal tablero f1 (c1+1) f2 c2 n
                                                            )else 
                                                                false
                                                        )else (
                                                            if (n = 0) then( 
                                                                comprobarHorizontal tablero f1 c1 f2 (c2+1) 1
                                                            )else if ((getElem tablero f2 c2) = 0) then( 
                                                                comprobarHorizontal tablero f1 c1 f2 (c2+1) n
                                                            )else
                                                                false
                                                        );;

let rec comprobarVertical = fun tablero f1 c1 f2 c2 n -> if (f1 = f2) then true else
                                                        if (f1 < f2) then(
                                                            if (n = 0) then( 
                                                                comprobarVertical tablero (f1+1) c1 f2 c2 1
                                                            )else if ((getElem tablero f1 c1) = 0) then( 
                                                                comprobarVertical tablero (f1+1) c1 f2 c2 n
                                                            )else 
                                                                false
                                                        )else (
                                                            if (n = 0) then( 
                                                                comprobarVertical tablero f1 c1 (f2+1) c2 1
                                                            )else if ((getElem tablero f2 c2) = 0) then( 
                                                                comprobarVertical tablero f1 c1 (f2+1) c2 n
                                                            )else
                                                                false
                                                        );;


let rec comprobarDiagonal = fun tablero f1 c1 f2 c2 n tam_f tam_c -> if ((f1 = f2) && (c1 = c2)) then true
                                                                else if ((f1<=tam_f) & (f2<=tam_f) & (c1 <= tam_c) & (c2 <= tam_c)) then(
                                                                    if (f1 < f2) then(
                                                                        if (c1 < c2) then(
                                                                            if (n = 0) then(
                                                                                comprobarDiagonal tablero (f1+1) (c1+1) f2 c2 1 tam_f tam_c
                                                                            )else if ((getElem tablero f1 c1) = 0) then(
                                                                                comprobarDiagonal tablero (f1+1) (c1+1) f2 c2 n tam_f tam_c
                                                                            )else
                                                                                false
                                                                        )else( (*c1 > c2*)
                                                                            if (n = 0) then(
                                                                                comprobarDiagonal tablero (f1+1) (c1-1) f2 c2 1 tam_f tam_c
                                                                            )else if ((getElem tablero f1 c1) = 0) then(
                                                                                comprobarDiagonal tablero (f1+1) (c1-1) f2 c2 n tam_f tam_c
                                                                            )else(
                                                                                false
                                                                            )
                                                                        )
                                                                    )else( (*f1 > f2*)
                                                                        if (c1 < c2) then(
                                                                            if (n = 0) then(
                                                                                comprobarDiagonal tablero (f1-1) (c1+1) f2 c2 1 tam_f tam_c
                                                                            )else if ((getElem tablero f1 c1) = 0) then(
                                                                                comprobarDiagonal tablero (f1-1) (c1+1) f2 c2 n tam_f tam_c
                                                                            )else
                                                                                false
                                                                        )else( (*c1 > c2*)
                                                                            if (n = 0) then(
                                                                                comprobarDiagonal tablero (f1-1) (c1-1) f2 c2 1 tam_f tam_c
                                                                            )else if ((getElem tablero f1 c1) = 0) then(
                                                                                comprobarDiagonal tablero (f1-1) (c1-1) f2 c2 n tam_f tam_c
                                                                            )else
                                                                                false
                                                                        )
                                                                    )
                                                                ) else false;;



let comprobarPrimerNivel2 = fun tablero f1 c1 f2 c2 -> if (f1 = f2 && c1 = c2) then false else
                                              if ((getElem tablero f1 c1) = 0 || (getElem tablero f2 c2) = 0) then false else
                                              if ((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) 
                                              then(
                                                if ((abs (f1-f2) <=1) && (abs (c1-c2) <=1)) then(
                                                    (*Llama la funcion de sumar puntos*)
                                                    sumarPuntos 1;

                                                    (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                    (realizarMovidad f1 c1 f2 c2);

                                                    true
                                                ) else if (f1 = f2) then(
                                                    if (((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) & (comprobarHorizontal tablero f1 c1 f2 c2 0)) then(
                                                        (*Llama la funcion de sumar puntos*)
                                                        sumarPuntos 4;

                                                        (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                        (realizarMovidad f1 c1 f2 c2);

                                                        true
                                                    ) else false
                                                ) else if (c1 = c2) then(
                                                    print_endline "Hola";
                                                    if (((getElem tablero f1 c1) = (getElem tablero f2 c2) || (getElem tablero f1 c1) + (getElem tablero f2 c2) = 10) & (comprobarVertical tablero f1 c1 f2 c2 0)) then(
                                                        (*Llama la funcion de sumar puntos*)
                                                        sumarPuntos 4;

                                                        (*Coloca los 0 en las casillas jugadas y actuliza el tablero*)
                                                        (realizarMovidad f1 c1 f2 c2);

                                                        true
                                                    ) else false
                                                ) else false
                                              )
                                              else false;;


(*
                *
      - +       *    - -
      f1c1      *    f1c1 
                *
***************f2c2******************
      + +       *    + -
      f1c1      *    f1c1
                *
                *
*)


(*
                *
      - +       *    - -
      f2c2      *    f2c2 
                *
***************f1c1******************
      + +       *    + -
      f2c2      *    f2c2
                *
                *
*)


