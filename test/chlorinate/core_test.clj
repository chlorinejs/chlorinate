(ns chlorinate.core-test
  (:use [chlorinate.core]
        [clojure.test]))

(deftest literal-tests
  (is (= (js->cl2 "x;")
         '(x)))
  (is (= (js->cl2 "  2;")
         '(2))))

(deftest variable-declaration-tests
  (is (= (js->cl2 "var x = 1")
         '((def x 1))))

  (is (= (js->cl2 "x = 1, y = 2;")
         '[(do (set! x 1) (set! y 2) undefined)])))

(deftest binary-expression-tests
  (is (= (js->cl2 "10 in []")
         '[(contains? [] 10)]))
  (is (= (js->cl2 "1 + 2;")
         '((+ 1 2))))
  (is (= (js->cl2 "1 + 2 + 3;")
         '((+ 1 2 3))))
  (is (= (js->cl2 "(1 + 2) + (3 + (5 + 4));")
         '[(+ 1 2 3 5 4)]))
  (is (= (js->cl2 "1 + 2 + 3 + 4;")
         '((+ 1 2 3 4))))
  (is (= (js->cl2 "1 - 2 - 3 - 4;")
         '[(- 1 2 3 4)]))
  (is (= (js->cl2 "1 - 2 - (3 - 4) - (5 + 1);")
         '[(- 1 2 (- 3 4) (+ 5 1))]))
  (is (= (js->cl2 "(1 + 2) * 3;")
         '((* (+ 1 2) 3))))
  (is (= (js->cl2 "1 | 2")
         '((bit-or 1 2))))
  (is (= (js->cl2 "1 & 2")
         '((bit-and 1 2)))))

(deftest call-expression-tests
  (is (= (js->cl2 "doThat();")
         '((doThat)))))

(deftest block-statement-tests
  (is (= (js->cl2 "{a; b;}")
         '((do a b undefined)))))

(deftest if-statement-test
  (is (= (js->cl2 "if (x) doThat();")
         '((if x (doThat)))))

  (is (= (js->cl2 "if (x) { doThat();}")
         '((if x (doThat)))))
   (is (= (js->cl2 "x && y? 1 : 2")
         '((if (and x y) 1 2)))))

(deftest swith-statement-test
  (is (= (js->cl2 "switch (answer) { case 42: bingo() }")
         '((case answer 42 (bingo)))))
  (is (= (js->cl2 "switch (answer) { case 42: bingo();
case 43: dingo();}")
         '((case answer 42 (bingo) 43 (dingo)))))
  (is (= (js->cl2 "switch (answer) { case 42: bingo();
default: dingo();}")
         '((case answer 42 (bingo) (dingo))))))

(deftest logical-expression-test
  (is (= (js->cl2 "x||y")
         '((or x y)))))

;; use index base to detect Regexp and such stuffs

(deftest regular-expression-test
  (is (= (str (js->cl2 "x = /[a-z]/i"))
         (str '[(set! x #"/[a-z]/i")]))))

(deftest multi-line-tests
  (is (= (js->cl2 "x = 1; y = 2;")
         '((set! x 1) (set! y 2)))))

(deftest function-expression
  (is (= (js->cl2 "(function(){ return 1;})")
         '((fn [] 1))))
  (is (= (js->cl2 "(function x (){ return 1;})")
         '[(fn x [] 1)]))
  (is (= (js->cl2 "(function(){ return x + y + z;})")
         '[(fn [] (+ x y z))])))

(deftest member-expression-tests
  (is (= (js->cl2 "universe.milkyway.solarsystem.Earth")
         '((-> universe :milkyway :solarsystem :Earth))))
  (is (= (js->cl2 "a.b.e(3).f(4).g().h().a")
         '[(-> (.. (-> a :b) (e 3) (f 4) g h) :a)])))

(deftest assignment-expression-test
  (is (= (js->cl2 "x = {answer: 42};")
         '((set! x {:answer 42}))))
  (is (= (js->cl2 "x+=y")
         '((set! x (+ x y))))))

(deftest get-attribute-tests
  (is (= (js->cl2 "x['a']")
         '[(get x :a)]))
  (is (= (js->cl2 "x[a]")
         '[(get x a)])))

(deftest for-statement-tests
  (is (= (js->cl2 "for (i=0; i<5; i++) {1}")
         '[(dofor [(set! i 0) (< i 5) (inc-after! i)] 1)]))
  (is (= (js->cl2 "for(x = 0;;);")
         '[(dofor [(set! x 0) nil nil] nil)]))
  (is (= (js->cl2 (str "for (_i = 0, _len = d.length; _i < _len; _i++) {}"))
         '[(dofor [(do (set! _i 0)
                       (set! _len (-> d :length))
                       undefined)
                   (< _i _len)
                   (inc-after! _i)]
                  undefined)])))

(deftest for-in-statement-tests
  (is (= (js->cl2 "var x=0;var o= {'a': 1, 'b': 2};for (k in o) {x += o[k]}")
         '[(def x 0)
           (def o {"b" 2, "a" 1})
           (dokeys
            [k o]
            (set! x (+ x (get o k))))]))
  (is (= (js->cl2
          (str "for (var i = function() { return 10 in [] } in list)"
               "process(x);"))
         '[(dokeys
            [(def i (fn [] (contains? [] 10))) list]
            (process x))])))

(deftest while-statement-tests
  (is (= (js->cl2
          "while (x < 10) { x++; y--; }")
         '((while (< x 10) (do (inc-after! x)
                               (dec-after! y))))))
  (is (= (js->cl2
          "do keep(); while (true)")
         '((do-while true (keep))))))

(deftest unary-expression-test
  (is (= (js->cl2 "~5")
         '((bit-not 5))))
  (is (= (js->cl2 "!true")
         '((not true))))
  (is (= (js->cl2 "!x")
         '((not x)))))

(deftest update-expression-tests
  (is (= (js->cl2 "i++")
         '((inc-after! i))))
  (is (= (js->cl2 "--x")
         '((dec! x)))))

(deftest labeled-statement-test
  (is (= (js->cl2 "A: while (true) { 1 }")
         '[(do
             (inline "/* Unsupport type: LabeledStatement */")
             (inline "\"A:\n    while (true) {\n        1;\n    }\""))])))

(deftest new-expression-tests
  (is (= (js->cl2 " new person('John',50,'blue');")
         '[(new person "John" 50 "blue")])))

(deftest this-expression-tests
  (is (= (js->cl2 "this")
         '[this])))

(deftest try-catch-finally-expression-tests
  (is (= (js->cl2 (str "try { doThat(); }"
                         " catch (e) { say(e) }"
                         " finally { cleanup(stuff) }"))
         '[(try (doThat)
                (catch e
                    (say e))
                (finally
                  (cleanup stuff)))]))
  (is (= (js->cl2 (str "try { doThat(); } catch (e) { say(e) } "))
         '[(try (doThat) (catch e (say e)))]))
  (is (= (js->cl2 (str "try { doThat(); } finally { cleanup(stuff) }"))
         '[(try (doThat) (finally (cleanup stuff)))])))

(deftest throw-statement-tests
  (is (= (js->cl2 "throw x;")
         '[(throw x)]))
  (is (= (js->cl2 "throw x * y")
         '[(throw (* x y))]))
  (is (= (js->cl2 "throw { message: \"Error\"}")
         '[(throw {:message "Error"})])))

(deftest break-and-continue-statement-tests
  (is (= (js->cl2 "while (true) { break }")
         '[(while true break)]))
  (is (= (js->cl2 "while (true) { continue }")
         '[(while true continue)])))
