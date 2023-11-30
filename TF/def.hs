data Expr = Num Int
          | Var Variable
          | Apply Expr Expr         -- 関数適用
          | Bexpr BinOp Expr Expr   -- 二項演算
          | Rexpr RelOp Expr Expr   -- 関係演算 ><
          | Fun Variable Expr       -- 関数抽象 lambda
          | Let Decl Expr           -- let
          | Letrec Decl Expr        -- letrec
          | If Expr Expr Expr       -- if
          | Pr Expr                 -- ! (for debug)
          deriving (Eq, Show)

type Prog = Expr

type Variable = [Char]

data Decl = Decl Variable Expr
            deriving(Eq, Show)

data BinOp = Add | Sub | Mul | deriving
             deriving(Eq, Show)

data RelOp = MoreThan | MoreThanEqual |
             LessThan | LessThanEqual
            deriving(Eq, Show)


-- 値
data Val = VInt Int
         | VBool Bool
         | VClosure Variable Expr Env -- 関数値（クロージャ）：仮引数 本体 環境
         deriving(Eq, Show)

unwrapInt :: Val -> Int
unwrapInt (VInt n) = n
unwrapInt (VBool b) = error "not an integer"
unwrapInt (VClosure v e x) = error "not an interger"

unwrapBool :: Val -> Bool
unwrapBool (VBool b) = b
unwrapBool (VInt n) = error "not an boolean"
unwrapBool (VClosure v e x) = error "not an boolean"

-- 環境
type Env = Assoc Variable Val -- 変数名と変数値

emptyEnv :: Env
emptyEnv = emptyAssoc

lookupEnv :: Variable -> Env -> Maybe Val -- 環境から変数値を探す
lookupEnv v env = lookupAssoc v env

updateEnv :: Variable -> Val -> Env -> Env -- 環境を更新した新しい環境を返す
updateEnv v val env = updateAssoc v val env