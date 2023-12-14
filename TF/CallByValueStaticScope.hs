expval :: Expr -> Env -> Val -- 式を環境のもとで評価
expval (Num n) env = n `seq` VInt n
expval (Var x) env = getval x (lookupEnv x env)
expval (Apply e1 e2) env = f `seq` v `seq` expval body newenv
                           where f = expval e1 env
                                 v = expval e2 env
                                 VClosure x body env' = f -- pettern match
                                 newenv = updateEnv x v env'
expval (Bexpr o e1 e2) env = v1 `seq` v2 `seq` r `seq` VInt r
                             where v1 = expval e1 env
                                   v2 = expval e2 env
                                   r  = binop o (unwrapInt v1) (unwrapInt v2)
expval (Rexpr o e1 e2) env = v1 `seq` v2 `seq` r `seq` VBool r
                             where v1 = expval e1 env
                                   v2 = expval e2 env
                                   r  = relop o (unwrapInt v1) (unwrapInt v2)
expval (Fun x e) env = VClosure x e env
expval (Let (Decl x e1) e2) env = v `seq` expval e2 newenv
                                  where v = expval e1 env
                                        newenv = updateEnv x v env
expval (Letrec (Decl x e1) e2) env = v `seq` expval e2 newenv
                                     where v = expval e1 newenv
                                           newenv = updateEnv x v env
expval (If e1 e2 e3) env = v `seq` expval (if unwrapBool v then e2 else e3) env
                           where v = expval e1 env

getval :: Variable -> Maybe Val -> Val
getval var (Just val) = val
getval var Nothing = error ("getval: " ++ var)

binop :: BinOp -> Int -> Int -> Int
binop Add m n = m + n
binop Sub m n = m - n
binop Mul m n = m * n
binop Div m n = div m n

relop :: RelOp -> Int -> Int -> Int
relop Equal m n = (m == n)
relop NotEqual m n = (m != n)
relop LessThan m n = (m < n)
relop LessThanEqual m n = (m <= n)