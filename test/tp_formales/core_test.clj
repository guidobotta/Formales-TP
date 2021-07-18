(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest generar-signo-test
  (testing "Funcion: generar-signo"
    (is (= ((generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)) '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= ((generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)) '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= ((generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= ((generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*)) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]))
    (is (= ((generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)) '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]))
    ))

(deftest generar-operador-relacional-test
  (testing "Funcion: generar-operador-relacional"
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]))
   (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=) '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]))
    ))

;; (deftest -test
;;   (testing
;;    (is (=))))
