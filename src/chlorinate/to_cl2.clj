(ns chlorinate.to-cl2
  (:use [chlorinate.esparse :only [esgenerate]]))

(defn raw-inline
  "Generate inline javascript code from s-expression map."
  ([m]
     (list 'inline (clojure.string/replace (pr-str (esgenerate m))
                                           "\\n" "\n")))
  ([msg m]
     (list 'do
           (list 'inline msg)
           (list 'inline (clojure.string/replace (pr-str (esgenerate m))
                                                 "\\n" "\n")))))

(def chain?
  "An atom used in expressions with '> and/or '< to check if the
 current sequence is an increasing/decreasing one."
  (atom true))

(defn operator->cl2
  "Convert an javascript operator to a Clojure one."
  [s]
  (case s
    "in"  'contains?
    "||"  'or
    "&&"  'and
    "!"   'not
    "%"   'rem
    "&"   'bit-and
    "|"   'bit-or
    "^"   'bit-xor
    "~"   'bit-not
    "<<"  'bit-shift-left
    ">>"  'bit-shift-right
    ">>>" 'bit-shift-right-zero-fill
    ;; + - * /
    (symbol s)))

(defn remove-undefined [c]
  "Remove `undefined` expressions in the end of a `do` block"
  (if (seq? c)
    ;; force evaluate lazy seq
    (let [coll (apply list c)]
      (if (= (first coll) 'do)
        (let [main-coll
              (if (= 'undefined (last coll))
                (take (dec (count coll)) coll)
                coll)]
          (if (> (count main-coll) 2)
            main-coll
            (second main-coll)))
        coll))
    c))

(defmulti to-cl2
  "Generate Clojure code from a map of s-expressions"
  :type)

(defmethod to-cl2 "Literal"
  [m]
  (if (and (map? (:value m))
           (= "RegularExpression"
              (:type (:value m))))
    (re-pattern (:value (:value m)))
    (:value m)))

(defmethod to-cl2 "FunctionDeclaration"
  [m]
  (if-let [function-name (:id m)]
    (let [params (vec (map to-cl2 (:params m)))]
      (list 'defn
            (to-cl2 function-name)
            params
            (to-cl2 (:body m))))))

(defmethod to-cl2 "FunctionExpression"
  [m]
  (concat ['fn]
          (if-let [func-name (:id m)] [(to-cl2 func-name)])
          [(vec (map to-cl2 (:params m)))
           (to-cl2 (:body m))]))

(defmethod to-cl2  "Identifier"
  [m] (symbol (:name m)))

(defmethod to-cl2 "VariableDeclarator"
  [m]
  (list 'def
        (to-cl2 (:id m))
        (to-cl2 (:init m))))

(defmethod to-cl2 "VariableDeclaration"
  [m]
  (let [declr (:declarations m)]
    (if (> (count declr) 1)
      (cons 'do (map to-cl2 declr))
      (to-cl2 (first declr)))))

(defmethod to-cl2 "BinaryExpression"
  ;; + - * / % | &
  [m]
  (cond
    (#{">" "<"} (:operator m))
    (do (swap! chain? (constantly true))
        (let [chain (concat
                     (if (and (= (:type (:left m))
                                 "BinaryExpression")
                              (#{">" "<"} (:operator (:left m))))

                       (if (= (:operator m)
                              (:operator (:left m)))
                         (to-cl2 (:left m))
                         (swap! chain? (fn [_] false)))

                       [(symbol (:operator m)) (to-cl2 (:left m))])
                     [(to-cl2 (:right m))])]
          (if @chain?
            chain
            (raw-inline "/* not a chain */" m))))

    (#{"-"} (:operator m))
    (concat
     (if (and (= (:type (:left m))
                 "BinaryExpression")
              (#{"-"} (:operator (:left m))))

       (if (= (:operator m)
              (:operator (:left m)))
         (to-cl2 (:left m)))

       [(symbol (:operator m)) (to-cl2 (:left m))])
     [(to-cl2 (:right m))])

    (#{"+" "*"} (:operator m))
    (concat
     [(symbol (:operator m))]
     (let [left (to-cl2 (:left  m))]
       (if (and (seq? left)
                (= (first left)
                   (symbol (:operator m))))
         (rest left)
         [left]))
     (let [right (to-cl2 (:right  m))]
       (if (and (seq? right)
                (= (first right)
                   (symbol (:operator m))))
         (rest right)
         [right])))

    (= "in" (:operator m))
    (list (operator->cl2 (:operator m))
          (to-cl2 (:right m))
          (to-cl2 (:left m)))

    :default
    (list (operator->cl2 (:operator m))
          (to-cl2 (:left m))
          (to-cl2 (:right m)))))

(defmethod to-cl2 "UpdateExpression"
  [m]

  (let [a (to-cl2 (:argument m))]
    (if (:prefix m)
      (case (:operator m)
        "++" (list 'inc! a)
        "--" (list 'dec! a))
      (case (:operator m)
        "++" (list 'inc-after! a)
        "--" (list 'dec-after! a)))))

(defmethod to-cl2 "ExpressionStatement"
  [m]
  (to-cl2 (:expression m)))

(defmethod to-cl2 "AssignmentExpression"
  [m]
  (let [x (to-cl2 (:left m))
        y (to-cl2 (:right m))]
    (list 'set!
          x
          (case (:operator m)
            "="  y
            "+=" (list '+ x y)
            "-=" (list '- x y)
            "*=" (list '* x y)
            "/=" (list '/ x y)
            ))))

(defmethod to-cl2 "ArrayExpression"
  [m]
  (vec (map to-cl2 (:elements m))))

(defmethod to-cl2 nil
  [_] nil)

(defmethod to-cl2 "EmptyStatement"
  [_] nil)

(defmethod to-cl2 "UnaryExpression"
  [m]
  (list (operator->cl2 (:operator m))
        (to-cl2 (:argument m))))

(defmethod to-cl2 "ObjectExpression"
  [m]
  (if (= (:properties m) [])
    {}
    (apply merge (map to-cl2 (:properties m)))))

(defmethod to-cl2 "Property"
  [m]
  {(if (= "Identifier" (:type (:key m)))
     (keyword (:name (:key m)))
     (to-cl2 (:key m)))
   (to-cl2 (:value m))})

(defmethod to-cl2 "CallExpression"
  [m]
  (if (= "MemberExpression"
         (:type (:callee m)))
    (let [head (to-cl2 (:object (:callee m)))
          tail (let [method (to-cl2 (:property (:callee m)))
                     args (map to-cl2 (:arguments m))]
                 (if (= args [])
                   method
                   (concat [method]
                           args)))]
      (concat
       (let [callee (:object (:callee m))]
         (if (and (= (:type callee) "CallExpression")
                  (= (:type (:callee callee)) "MemberExpression"))
           head
           ['.. head]))
       [tail]))

    (let [func (to-cl2 (:callee m))
          args (map to-cl2 (:arguments m))]
      (concat [func] args))))

(defmethod to-cl2 "ReturnStatement"
  [m]
  (to-cl2 (:argument m)))

(defmethod to-cl2 "BlockStatement"
  [m]
  (let [before-last-expr (map to-cl2
                           (for [before-return (:body m)
                                 :while (not= (:type before-return)
                                              "ReturnStatement")]
                             before-return))
        last-expr (if-let [return-statement
                           (first (for [return-statements (:body m)
                                        :when (= (:type return-statements)
                                                 "ReturnStatement")]
                                    return-statements))]
                    (to-cl2 return-statement)
                    'undefined)]
    (if (pos? (count before-last-expr))
      (concat '(do) before-last-expr [last-expr])
      last-expr)))

(defmethod to-cl2 "IfStatement"
  [m]
  (concat ['if
           (to-cl2 (:test m))
           (remove-undefined (to-cl2 (:consequent m)))]

          (if (:alternate m)
            [(remove-undefined (to-cl2 (:alternate m)))])))

(defmethod to-cl2 "ConditionalExpression"
  [m]
  (concat ['if
           (to-cl2 (:test m))
           (remove-undefined (to-cl2 (:consequent m)))]

          (if (:alternate m)
            [(remove-undefined (to-cl2 (:alternate m)))])))

(defmethod to-cl2 "SwitchStatement"
  [m]
  (concat
   ['case (to-cl2 (:discriminant m))]
   (apply concat (map to-cl2 (:cases m)))))

(defmethod to-cl2 "SwitchCase"
  [m]
  (let [expr (if (= 1 (count (:consequent m)))
               (to-cl2 (first (:consequent m)))
               (cons 'do (map to-cl2 (:consequent m))))]
    (if-let [test-case (to-cl2 (:test m))]
      ;; if this is not a 'default' case:
      [test-case expr]
      [expr])))

(defmethod to-cl2 "LogicalExpression"
  [m]
  (list (operator->cl2 (:operator m))
        (to-cl2 (:left m))
        (to-cl2 (:right m))))

(defmethod to-cl2 "NewExpression"
  [m]
  (filter #(not (nil? %))
          (concat ['new (to-cl2 (:callee m))]
                  (map to-cl2 (:arguments m)))))

(defmethod to-cl2 "MemberExpression"
  [m]
  (if (:computed m)
    (list 'get
          (to-cl2 (:object m))
          (let [prop (to-cl2 (:property m))]
            (if (string? prop)
              (keyword prop)
              prop)))
    (concat
     (if (= (:type (:object m)) "MemberExpression")
       (to-cl2 (:object m))
       ['->
        (to-cl2 (:object m))])
     [(keyword (to-cl2 (:property m)))])))

(defmethod to-cl2 "DoWhileStatement"
  [m]
  (list 'do-while
        (to-cl2 (:test m))
        (remove-undefined (to-cl2 (:body m)))))

(defmethod to-cl2 "WhileStatement"
  [m]
  (list 'while
        (to-cl2 (:test m))
        (remove-undefined (to-cl2 (:body m)))))

(defmethod to-cl2 "ForStatement"
  [m]
  (list 'dofor
        [(to-cl2 (:init m))
         (to-cl2 (:test m))
         (to-cl2 (:update m))]
        (remove-undefined (to-cl2 (:body m)))))

(defmethod to-cl2 "ForInStatement"
  [m]
  (list 'dokeys
        [(to-cl2 (:left m))
         (to-cl2 (:right m))]
        (remove-undefined (to-cl2 (:body m)))))

(defmethod to-cl2 "ThisExpression"
  [m]
  'this)

(defmethod to-cl2 "TryStatement"
  [m]
  (concat ['try
           (remove-undefined (to-cl2 (:block m)))]
          (map to-cl2 (:handlers m))
          (if-let [finalizer (:finalizer m)]
            [(list 'finally
                   (remove-undefined (to-cl2 finalizer)))])))

(defmethod to-cl2 "CatchClause"
  [m]
  (list 'catch
        (to-cl2 (:param m))
        (remove-undefined (to-cl2 (:body m)))))

(defmethod to-cl2 "ThrowStatement"
  [m]
  (list 'throw
        (to-cl2 (:argument m))))

(defmethod to-cl2 "BreakStatement"
  [m]
  'break)

(defmethod to-cl2 "ContinueStatement"
  [m]
  'continue)

(defmethod to-cl2 "SequenceExpression"
  [m]
  (concat ['do]
          (map to-cl2 (:expressions m))
          ['undefined]))

(defmethod to-cl2 :default
  [m]
  (raw-inline (format "/* Unsupport type: %s */" (:type m))
              m))
