-- a)
data VariableName = X | Y deriving Show

getValue :: VariableName -> Int
getValue X = 5
getValue Y = 13

-- b)
data Expression = Constant  Int
                | Variable  VariableName
                | Add       Expression Expression
                | Multiply  Expression Expression
                deriving Show

-- c)
evaluate :: Expression -> Int
evaluate (Constant c)      = c
evaluate (Variable v)      = getValue v
evaluate (Add e1 e2)       = evaluate e1 + evaluate e2
evaluate (Multiply e1 e2)  = evaluate e1 * evaluate e2

-- d)
tryOptimize :: Expression -> Expression
tryOptimize (Add      (Constant c1) (Constant c2)) = Constant (c1 + c2)
tryOptimize (Multiply (Constant c1) (Constant c2)) = Constant (c1 * c2)
tryOptimize e = e

-- e)
evaluatePartially :: Expression -> Expression
evaluatePartially (Add e1 e2)      = tryOptimize (Add
                                                   (evaluatePartially e1)
                                                   (evaluatePartially e2))
evaluatePartially (Multiply e1 e2) = tryOptimize (Multiply
                                                   (evaluatePartially e1)
                                                   (evaluatePartially e2))
evaluatePartially e                = e

