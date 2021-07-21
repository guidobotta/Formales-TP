(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest generar-signo-test
  (testing "Funcion: generar-signo"
    (is (= (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-) '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+) '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]))
    ))

(deftest generar-operador-relacional-test
  (testing "Funcion: generar-operador-relacional"
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '<>) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET NEQ]]))
    ))

(deftest fixup-test
  (testing "Funcion: fixup"
   (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1) '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]))
   (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]))
   (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]))
    ))

(deftest buscar-coincidencias-test
  (testing "Funcion: buscar-coincidencias"
    (is (= (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]) '([X VAR 0] [X VAR 2])))
    (is (= (buscar-coincidencias '[nil () [CALL Y] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]) '([Y VAR 1] [Y VAR 3])))
    ))

(deftest generar-test
  (testing "Funcion: generar"
   (is (= (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT) '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]]))
   (is (= (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0) '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]]))
   (is (= (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT) '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]))
   (is (= (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0) '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]))
    ))

(deftest dump-test
  (testing "Funcion: dump"
   (is (= (with-out-str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])) "0 [PFM 0]\n1 [PFI 2]\n2 MUL\n3 [PFI 1]\n4 ADD\n5 NEG\n"))
   (is (= (with-out-str (dump '[HLT])) "0 HLT\n"))
   (is (= (with-out-str (dump nil)) ""))
    ))

(deftest aplicar-relacional-test
  (testing "Funcion: aplicar-relacional"
   (is (= (aplicar-relacional > [7 5]) '[1]))
   (is (= (aplicar-relacional > [4 7 5]) '[4 1]))
   (is (= (aplicar-relacional = [4 7 5]) '[4 0]))
   (is (= (aplicar-relacional not= [4 7 5]) '[4 1]))
   (is (= (aplicar-relacional < [4 7 5]) '[4 0]))
   (is (= (aplicar-relacional <= [4 6 6]) '[4 1]))
   (is (= (aplicar-relacional <= '[a b c]) '[a b c]))
    ))

(deftest aplicar-aritmetico-test
  (testing "Funcion: aplicar-aritmetico"
   (is (= (aplicar-aritmetico + [1 2]) '[3]))
   (is (= (aplicar-aritmetico - [1 4 1]) '[1 3]))
   (is (= (aplicar-aritmetico * [1 2 4]) '[1 8]))
   (is (= (aplicar-aritmetico / [1 2 4]) '[1 0]))
   (is (= (aplicar-aritmetico + nil) nil))
   (is (= (aplicar-aritmetico + []) '[]))
   (is (= (aplicar-aritmetico + [1]) '[1]))
   (is (= (aplicar-aritmetico 'hola [1 2 4]) '[1 2 4]))
   (is (= (aplicar-aritmetico count [1 2 4]) '[1 2 4]))
   (is (= (aplicar-aritmetico + '[a b c]) '[a b c]))
   ))

(deftest expresion-test
  (testing "Funcion: expresion"
   (is (= (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]) ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))
   (is (= (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]) ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '+ (symbol "(") 'X '* 2 '+ 1 (symbol ")") ] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL [PFI 1] ADD]]))
   (is (= (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]) ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '- (symbol "(") 'X '* 2 '+ 1 (symbol ")") ] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]))
   ))

(deftest termino-test
  (testing "Funcion: termino"
   (is (= (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]) ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))
   (is (= (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]) ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") 'X '* 2] :sin-errores '[[0] [[X VAR 0]]] 1 [['PFM 0] ['PFI 2] 'MUL]]))
   ))

(deftest procesar-signo-unario-test
  (testing "Funcion: procesar-signo-unario"
   (is (= (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]) ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))
   (is (= (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]) [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))
   (is (= (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]) [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") '+] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))
   (is (= (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]) [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") '-] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))
   ))

(deftest declaracion-var-test
  (testing "Funcion: declaracion-var"
    (is (= (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]]) ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]]))
    (is (= (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]) ['BEGIN (list 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";")] :sin-errores [[0] '[[X VAR 0] [Y VAR 1]]] 2 '[[JMP ?]]]))
    ))

(deftest inicializar-contexto-local-test
  (testing "Funcion: inicializar-contexto-local"
    (is (= (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]) '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]))
    (is (= (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]) '[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]))
    ))

(deftest cargar-var-en-tabla-test
  (testing "Funcion: cargar-var-en-tabla"
    (is (= (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]) '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]))
    (is (= (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]]) '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]))
    (is (= (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]) '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]))
    ))

(deftest ya-declarado-localmente?-test
  (testing "Funcion: ya-declarado-localmente?"
    (is (= (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]]) true))
    (is (= (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]]) false))
    (is (= (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]]) false))
    (is (= (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]]) true))
    ))

(deftest cadena?-test
  (testing "Funcion: cadena?"
    (is (= (cadena? "'Hola'") true))
    (is (= (cadena? "Hola") false))
    (is (= (cadena? "'Hola") false))
    (is (= (cadena? 'Hola) false))
    (is (= (cadena? "'*****************************************************************'") true))
    ))

(deftest identificador?-test
  (testing "Funcion: identificador?"
    (is (= (identificador? 2) false))
    (is (= (identificador? 'V2) true))
    (is (= (identificador? "V2") true))
    (is (= (identificador? 'CALL) false))
    ))

(deftest palabra-reservada?-test
  (testing "Funcion: palabra-reservada?"
    (is (= (palabra-reservada? 'CALL) true))
    (is (= (palabra-reservada? "CALL") true))
    (is (= (palabra-reservada? 'ASIGNAR) false))
    (is (= (palabra-reservada? "ASIGNAR") false))
    ))

(deftest a-mayusculas-salvo-strings-test
  (testing "Funcion: a-mayusculas-salvo-strings"
    (is (= (a-mayusculas-salvo-strings "  const Y = 2;") "  CONST Y = 2;"))
    (is (= (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');") "  WRITELN ('Se ingresa un valor, se muestra su doble.');"))
    ))