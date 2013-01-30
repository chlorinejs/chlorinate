(ns chlorinate.to-cl2-test
  (:use [chlorinate.to-cl2]
        [clojure.test]))

(deftest remove-undefined-test
  (is (= (remove-undefined '(do (doThat) undefined))
         '(doThat)))
  (is (= (remove-undefined "foo")
         "foo")))

(deftest literal-tests
  (is (= (to-cl2 {:type "Literal" :value 2})
         2))
  (is (= (to-cl2 {:type "Identifier", :name "x"})
         'x)))

(deftest variable-declaration-tests
  (is (= (to-cl2 {:type "VariableDeclarator",
                  :id {:type "Identifier",
                       :name "x"},
                  :init {:type "Literal",
                         :value 1}})
         '(def x 1)))
  (is (= (to-cl2 {:type "VariableDeclaration",
                  :declarations [{:type "VariableDeclarator",
                                  :id {:type "Identifier",
                                       :name "x"},
                                  :init {:type "Literal",
                                         :value 1}}],
                  :kind "var"})
         '(def x 1))))

(deftest binary-expression-tests
  (is (= (to-cl2
          {:type "BinaryExpression",
           :operator "+",
           :left {:type "Literal", :value 1},
           :right {:type "Literal", :value 2}})
         '(+ 1 2)))
  (is (= (to-cl2
          {:type "BinaryExpression",
           :operator "*",
           :left {:type "BinaryExpression",
                  :operator "+",
                  :left {:type "Literal", :value 1},
                  :right {:type "Literal", :value 2}},
           :right {:type "Literal", :value 3}})
         '(* (+ 1 2) 3))))

(deftest expression-statement-tests
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "BinaryExpression",
                               :operator "*",
                               :left {:type "BinaryExpression",
                                      :operator "+",
                                      :left {:type "Literal", :value 1},
                                      :right {:type "Literal", :value 2}},
                               :right {:type "Literal", :value 3}}})
         '(* (+ 1 2) 3)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "BinaryExpression",
                               :operator "<<",
                               :left {:type "BinaryExpression",
                                      :operator "+",
                                      :left {:type "Literal",
                                             :value 4},
                                      :right {:type "Literal",
                                              :value 5}},
                               :right {:type "Literal", :value 6}}})
         '(bit-shift-left (+ 4 5) 6)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "BinaryExpression",
                               :operator "+",
                               :left {:type "BinaryExpression",
                                      :operator "+",
                                      :left {:type "Literal",
                                             :value 1},
                                      :right {:type "Literal",
                                              :value 2}},
                               :right {:type "Literal",
                                       :value 3}}})
         '(+ 1 2 3)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "AssignmentExpression",
                               :operator "=",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "ArrayExpression",
                                       :elements []}}})
         '(set! x [])))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "AssignmentExpression",
                               :operator "=",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "ArrayExpression",
                                       :elements
                                       [{:type "Literal",
                                         :value 42}]}}})
         '(set! x [42])))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "AssignmentExpression",
                               :operator "=",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "ArrayExpression",
                                       :elements [{:type "Literal",
                                                   :value 1}
                                                  {:type "Literal",
                                                   :value 2}
                                                  nil
                                                  {:type "Literal",
                                                   :value 3}]}}})
         '(set! x [1 2 nil 3])))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "AssignmentExpression",
                               :operator "=",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "ObjectExpression",
                                       :properties []}}})
         '(set! x {}))))

(deftest object-expression-tests
  (is (= (to-cl2 {:type "ObjectExpression",
                  :properties []})
         '{}))

  (is (= (to-cl2 {:type "ObjectExpression",
                  :properties
                  [{:type "Property",
                    :key {:type "Identifier",
                          :name "answer"},
                    :value {:type "Literal",
                            :value 42},
                    :kind "init"}]})
         {:answer 42})))

(deftest object-property-tests
  (is (= (to-cl2 {:type "Property",
                  :key {:type "Identifier",
                        :name "answer"},
                  :value {:type "Literal",
                          :value 42},
                  :kind "init"})
         {:answer 42})))

(deftest assignment-expression-test
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "AssignmentExpression",
                               :operator "=",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "ObjectExpression",
                                       :properties
                                       [{:type "Property",
                                         :key {:type "Identifier",
                                               :name "answer"},
                                         :value {:type "Literal",
                                                 :value 42},
                                         :kind "init"}]}}})
         '(set! x {:answer 42}))))

(deftest return-statement-tests
  (is (= (to-cl2
          {:type "BlockStatement",
           :body
           [{:type "ReturnStatement",
             :argument {:type "Identifier",
                        :name "m_width"}}]})
         'm_width)))

(deftest call-expression-tests
  (is (= (to-cl2 {:type "CallExpression",
                  :callee {:type "Identifier",
                           :name "doThat"},
                  :arguments []})
         '(doThat))))

(deftest block-statement-tests
  (is (= (to-cl2
          {:type "BlockStatement",
           :body [{:type "ExpressionStatement",
                   :expression {:type "Identifier",
                                :name "a"}}
                  {:type "ExpressionStatement",
                   :expression {:type "Identifier",
                                :name "b"}}]})
         '(do a b undefined)))
  (is (= (to-cl2 {:type "BlockStatement",
                  :body [{:type "ExpressionStatement",
                          :expression {:type "CallExpression",
                                       :callee {:type "Identifier",
                                                :name "doThat"},
                                       :arguments []}}]})
         '(do (doThat) undefined))))

(deftest remove-undefined-test
  (is (= '(do (doThat) undefined)
         (to-cl2 {:type "BlockStatement",
                  :body [{:type "ExpressionStatement",
                          :expression {:type "CallExpression",
                                       :callee {:type "Identifier",
                                                :name "doThat"},
                                       :arguments []}}]}))))

(deftest if-statement-test
  (is (= (to-cl2
          {:type "IfStatement",
           :test {:type "Identifier",
                  :name "x"},
           :consequent {:type "BlockStatement",
                        :body [{:type "ExpressionStatement",
                                :expression {:type "CallExpression",
                                             :callee {:type "Identifier",
                                                      :name "doThat"},
                                             :arguments []}}]},
           :alternate nil})
         '(if x (doThat))))
  (is (= (to-cl2
          {:type "ExpressionStatement",
           :expression {:type "ConditionalExpression",
                        :test {:type "LogicalExpression",
                               :operator "&&",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "Identifier",
                                       :name "y"}},
                        :consequent {:type "Literal",
                                     :value 1},
                        :alternate {:type "Literal",
                                    :value 2}}})
         '(if (and x y) 1 2))))

(deftest for-statement-test
  (is (= (to-cl2
          {:type "ForStatement",
           :init
           {:right {:type "Literal", :value 0},
            :type "AssignmentExpression",
            :left {:name "x", :type "Identifier"},
            :operator "="},
           :test
           {:right {:type "Literal", :value 42},
            :type "BinaryExpression",
            :left {:name "x", :type "Identifier"},
            :operator "<"},
           :update
           {:prefix false,
            :argument {:name "x", :type "Identifier"},
            :type "UpdateExpression",
            :operator "++"},
           :body
           {:expression
            {:arguments [{:name "x", :type "Identifier"}],
             :type "CallExpression",
             :callee {:name "process", :type "Identifier"}},
            :type "ExpressionStatement"}})
         '(dofor [(set! x 0) (< x 42) (inc-after! x)]
                (process x)))))

(deftest for-in-statement-test
  (to-cl2
   {:type "ForInStatement",
    :left {:name "x", :type "Identifier"},
    :right {:name "list", :type "Identifier"},
    :body
    {:expression
     {:arguments [{:name "x", :type "Identifier"}],
      :type "CallExpression",
      :callee {:name "process", :type "Identifier"}},
     :type "ExpressionStatement"},
    :each false})
  '(dokeys [x list] (process x)))

(deftest swith-statement-test
  (is (= (to-cl2
          {:type "SwitchStatement",
           :discriminant {:type "Identifier",
                          :name "answer"},
           :cases [{:type "SwitchCase",
                    :test {:type "Literal",
                           :value 42},
                    :consequent [{:type "ExpressionStatement",
                                  :expression {:type "CallExpression",
                                               :callee {:type "Identifier",
                                                        :name "bingo"},
                                               :arguments []}}]}
                   {:type "SwitchCase",
                    :test {:type "Literal",
                           :value 43},
                    :consequent [{:type "ExpressionStatement",
                                  :expression {:type "CallExpression",
                                               :callee {:type "Identifier",
                                                        :name "dingo"},
                                               :arguments []}}]}
                   {:type "SwitchCase",
                    :consequent [{:type "ExpressionStatement",
                                  :expression {:type "CallExpression",
                                               :callee {:type "Identifier",
                                                        :name "do-default"},
                                               :arguments []}}]}]})
         '(case answer
            42
            (bingo)

            43
            (dingo)

            (do-default)))))

(deftest logical-expression-test
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "LogicalExpression",
                               :operator "||",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "Identifier",
                                       :name "y"}}})
         '(or x y)))
  (is (= (to-cl2
          {:type "ExpressionStatement",
           :expression {:type "LogicalExpression",
                        :operator "||",
                        :left {:type "LogicalExpression",
                               :operator "||",
                               :left {:type "Identifier",
                                      :name "x"},
                               :right {:type "Identifier",
                                       :name "y"}},
                        :right {:type "Identifier",
                                :name "z"}}})
         '(or (or x y) z))))

(deftest regular-expression-test
  (is (= (to-cl2 {:type "VariableDeclaration",
                  :declarations [{:type "VariableDeclarator",
                                  :id {:type "Identifier",
                                       :name "x"},
                                  :init {:type "Literal",
                                         :value {}}}],
                  :kind "var"})
         '(def x {}))))

(deftest function-declaration-test
  (is (= (to-cl2
          {:type "FunctionDeclaration",
           :id {:type "Identifier",
                :name "a"},
           :params [],
           :defaults [],
           :body {:type "BlockStatement",
                  :body [{:type "ReturnStatement",
                          :argument {:type "Identifier",
                                     :name "x"}}]},
           :rest nil,
           :generator false,
           :expression false})
         '(defn a [] x))))

(deftest new-expression-statement-tests
  (is (= (to-cl2 {:type "ExpressionStatement",
                   :expression {:type "NewExpression",
                                :callee {:type "Identifier",
                                         :name "Button"},
                                :arguments []}})
         '(new Button)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                   :expression {:type "NewExpression",
                                :callee {:type "Identifier",
                                         :name "Button"},
                                :arguments []}})
         '(new Button)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "CallExpression",
                               :callee {:type "MemberExpression",
                                        :computed false,
                                        :object {:type "NewExpression",
                                                 :callee {:type "Identifier",
                                                          :name "foo"},
                                                 :arguments []},
                                        :property {:type "Identifier",
                                                   :name "bar"}},
                               :arguments []}})
         '(.. (new foo) bar)))

  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "NewExpression",
                               :callee {:type "NewExpression",
                                        :callee {:type "Identifier",
                                                 :name "foo"},
                                        :arguments []},
                               :arguments []}})
         '(new (new foo))))

  (is (= (to-cl2
          {:type "ExpressionStatement",
           :expression {:type "NewExpression",
                        :callee {:type "MemberExpression",
                                 :computed true,
                                 :object {:type "Identifier",
                                          :name "foo"},
                                 :property {:type "Identifier",
                                            :name "bar"}},
                        :arguments []}})
         '(new (get foo bar))))

  (is (= (to-cl2
          {:type "ExpressionStatement",
           :expression {:type "NewExpression",
                        :callee {:type "MemberExpression",
                                 :computed false,
                                 :object {:type "Identifier",
                                          :name "foo"},
                                 :property {:type "Identifier",
                                            :name "bar"}},
                        :arguments []}})
         '(new (-> foo :bar)))))

(deftest call-expression-tests
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "CallExpression",
                               :callee {:type "MemberExpression",
                                        :computed false,
                                        :object {:type "NewExpression",
                                                 :callee {:type "Identifier",
                                                          :name "foo"},
                                                 :arguments []},
                                        :property {:type "Identifier",
                                                   :name "bar"}},
                               :arguments []}})
         '(.. (new foo) bar)))
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "CallExpression",
                               :callee {:type "Identifier",
                                        :name "foo"},
                               :arguments []}})
         '(foo)))
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "CallExpression"
                               :callee {:type "Identifier",
                                        :name "foo"},
                               :arguments [{:type "Identifier",
                                            :name "bar"}
                                           {:type "Identifier",
                                            :name "baz"}]}})
         '(foo bar baz)))
  (is (= (to-cl2 {:type "ExpressionStatement",
                  :expression {:type "CallExpression",
                               :callee
                               {:type "MemberExpression",
                                :computed false,
                                :object
                                {:type "CallExpression",
                                 :callee {:type "MemberExpression",
                                          :computed false,
                                          :object {:type "CallExpression",
                                                   :callee {:type "Identifier",
                                                            :name "a"},
                                                   :arguments []},
                                          :property {:type "Identifier",
                                                     :name "foo"}},
                                 :arguments []},
                                :property {:type "Identifier",
                                           :name "listen"}},
                               :arguments [{:type "Identifier",
                                            :name "app"}]}})
         '(.. (a) foo (listen app)))))

(deftest member-expression-test
  (is (= (to-cl2
          {:type "ExpressionStatement",
           :expression {:type "MemberExpression",
                        :computed false,
                        :object {:type "MemberExpression",
                                 :computed false,
                                 :object {:type "MemberExpression",
                                          :computed false,
                                          :object {:type "Identifier",
                                                   :name "universe"},
                                          :property {:type "Identifier",
                                                     :name "milkyway"}},
                                 :property {:type "Identifier",
                                            :name "solarsystem"}},
                        :property {:type "Identifier",
                                   :name "Earth"}}})
         '(-> universe :milkyway :solarsystem :Earth))))

(deftest this-expression-test
  (is (= (to-cl2
          {:type "ThisExpression"})
         'this)))

(deftest try-catch-finally-expression-tests
  (is (= (to-cl2
          {:type "TryStatement",
           :block {:type "BlockStatement",
                   :body [{:type "ExpressionStatement",
                           :expression
                           {:type "CallExpression",
                            :callee {:type "Identifier",
                                     :name "doThat"},
                            :arguments []}}]},
           :guardedHandlers [],
           :handlers [{:type "CatchClause",
                       :param {:type "Identifier",
                               :name "e"},
                       :body {:type "BlockStatement",
                              :body [{:type "ExpressionStatement",
                                      :expression
                                      {:type "CallExpression",
                                       :callee {:type "Identifier",
                                                :name "say"},
                                       :arguments
                                       [{:type "Identifier",
                                         :name "e"}]}}]}}],
           :finalizer {:type "BlockStatement",
                       :body [{:type "ExpressionStatement",
                               :expression
                               {:type "CallExpression",
                                :callee {:type "Identifier",
                                         :name "cleanup"},
                                :arguments [{:type "Identifier",
                                             :name "stuff"}]}}]}})
         '(try (doThat)
               (catch e (say e))
               (finally (cleanup stuff))))))

(deftest throw-statement-tests
  (is (= (to-cl2 {:type "ThrowStatement",
                  :argument
                  {:type "ObjectExpression",
                   :properties [{:type "Property",
                                 :key {:type "Identifier",
                                       :name "message"},
                                 :value {:type "Literal",
                                         :value "Error"},
                                 :kind "init"}]}})
         '(throw {:message "Error"}))))

(deftest break-and-continue-statement-tests
  (is (= (to-cl2 {:type "WhileStatement",
                  :test {:type "Literal", :value true},
                  :body {:type "BlockStatement",
                         :body [{:type "BreakStatement",
                                 :label nil}]}})
         '(while true break)))
  (is (= (to-cl2 {:type "WhileStatement",
                  :test {:type "Literal", :value true},
                  :body {:type "BlockStatement",
                         :body [{:type "ContinueStatement",
                                 :label nil}]}})
         '(while true continue))))
