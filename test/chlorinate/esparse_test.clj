(ns chlorinate.esparse-test
  (:use [chlorinate.esparse]
        [clojure.test]))

(deftest literal-tests
  (is (= (esparse "x;")
         [{:type "ExpressionStatement",
           :expression {:type "Identifier", :name "x"}}]))
  (is (= (esparse " 2;")
         [{:type "ExpressionStatement",
           :expression {:type "Literal", :value 2}}])))

(deftest variable-declaration-tests
  (is (= (esparse "var x = 1")
         [{:type "VariableDeclaration",
           :declarations [{:type "VariableDeclarator",
                           :id {:type "Identifier", :name "x"},
                           :init {:type "Literal", :value 1}}],
           :kind "var"}]))
  (is (= (esparse "var x = 1, y = 2;")
         [{:type "VariableDeclaration",
           :declarations [{:type "VariableDeclarator",
                           :id {:type "Identifier", :name "x"},
                           :init {:type "Literal", :value 1}}
                          {:type "VariableDeclarator",
                           :id {:type "Identifier", :name "y"},
                           :init {:type "Literal", :value 2}}],
           :kind "var"}])))

(deftest binary-expression-tests
  (is (= (esparse "1 + 2")
         [{:type "ExpressionStatement",
           :expression {:type "BinaryExpression",
                        :operator "+",
                        :left {:type "Literal", :value 1},
                        :right {:type "Literal", :value 2}}}]))
  (is (= (esparse "1 | 2")
         [{:type "ExpressionStatement",
           :expression {:type "BinaryExpression",
                        :operator "|",
                        :left {:type "Literal", :value 1},
                        :right {:type "Literal", :value 2}}}]))
  (is (= (esparse "(1 + 2) * 3;")
         [{:type "ExpressionStatement",
           :expression {:type "BinaryExpression",
                        :operator "*",
                        :left {:type "BinaryExpression",
                               :operator "+",
                               :left {:type "Literal", :value 1},
                               :right {:type "Literal", :value 2}},
                        :right {:type "Literal", :value 3}}}]))
  (is (= (esparse "1 & 2")
         [{:type "ExpressionStatement",
           :expression {:type "BinaryExpression",
                        :operator "&",
                        :left {:type "Literal", :value 1},
                        :right {:type "Literal", :value 2}}}])))

(deftest object-expression-tests
  (is (= (esparse "x = {answer: 42}")
         [{:type "ExpressionStatement",
           :expression {:type "AssignmentExpression",
                        :operator "=",
                        :left {:type "Identifier", :name "x"},
                        :right {:type "ObjectExpression",
                                :properties [{:type "Property",
                                              :key {:type "Identifier",
                                                    :name "answer"},
                                              :value {:type "Literal",
                                                      :value 42},
                                              :kind "init"}]}}}]))
  (is (= (esparse "x = {}")
         [{:type "ExpressionStatement",
           :expression {:type "AssignmentExpression",
                        :operator "=",
                        :left {:type "Identifier",
                               :name "x"},
                        :right {:type "ObjectExpression",
                                :properties []}}}])))

(deftest this-expression-tests
  (is (= (esparse "this")
         [{:type "ExpressionStatement",
           :expression {:type "ThisExpression"}}])))

(deftest try-expression-tests
  (is (= (esparse (str "try { doThat(); }"
                       " catch (e) { say(e) }"
                       " finally { cleanup(stuff) }"))
         [{:type "TryStatement",
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
                               :expression {:type "CallExpression",
                                            :callee {:type "Identifier",
                                                     :name "cleanup"},
                                            :arguments [{:type "Identifier",
                                                         :name "stuff"}]}}]}}]
         )))

(deftest throw-statement-tests
  (is (= (esparse "throw x;")
         [{:type "ThrowStatement", :argument {:type "Identifier", :name "x"}}]))
  (is (= (esparse "throw x * y")
         [{:type "ThrowStatement",
           :argument {:type "BinaryExpression",
                      :operator "*",
                      :left {:type "Identifier",
                             :name "x"},
                      :right {:type "Identifier",
                              :name "y"}}}]))
  (is (= (esparse "throw { message: \"Error\"}")
         [{:type "ThrowStatement",
           :argument
           {:type "ObjectExpression",
            :properties [{:type "Property",
                          :key {:type "Identifier",
                                :name "message"},
                          :value {:type "Literal",
                                  :value "Error"},
                          :kind "init"}]}}])))

(deftest break-and-continue-statement-tests
  (is (= (esparse "while (true) { break }")
         [{:type "WhileStatement",
           :test {:type "Literal", :value true},
           :body {:type "BlockStatement",
                  :body [{:type "BreakStatement",
                          :label nil}]}}]))
  (is (= (esparse "while (true) { continue; }")
         [{:type "WhileStatement",
           :test {:type "Literal", :value true},
           :body {:type "BlockStatement",
                  :body [{:type "ContinueStatement",
                          :label nil}]}}])))

(deftest for-statement-tests
  (is (= (esparse (str "for (_i = 0, _len = d.length; _i < _len; _i++) {}"))
         [{:type "ForStatement",
           :init {:type "SequenceExpression",
                  :expressions [{:type "AssignmentExpression",
                                 :operator "=",
                                 :left {:type "Identifier",
                                        :name "_i"},
                                 :right {:type "Literal", :value 0}}
                                {:type "AssignmentExpression",
                                 :operator "=",
                                 :left {:type "Identifier", :name "_len"},
                                 :right {:type "MemberExpression",
                                         :computed false,
                                         :object {:type "Identifier",
                                                  :name "d"},
                                         :property {:type "Identifier",
                                                    :name "length"}}}]},
           :test {:type "BinaryExpression",
                  :operator "<",
                  :left {:type "Identifier",
                         :name "_i"},
                  :right {:type "Identifier",
                          :name "_len"}},
           :update {:type "UpdateExpression",
                    :operator "++",
                    :argument {:type "Identifier",
                               :name "_i"},
                    :prefix false},
           :body {:type "BlockStatement", :body []}}])))
