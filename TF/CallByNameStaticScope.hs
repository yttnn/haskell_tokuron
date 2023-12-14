-- 遅延関係
data Thunk = Thunk Expr Env -- 遅延された式の情報 式と環境を持つ
             deriving (Eq, Show)

-- 式を遅延する(Delayedで包んで返す)
delay :: Expr -> Env -> EnvVal
delay exp env = Delayed (Thunk exp env)

-- 値を求める
force :: EnvVal -> Val
force (Evaled v) = v
force (Delayed (Thunk exp env)) = expval exp env -- 遅延されてる時は、評価する

-- 遅延込の環境
data EnvVal = Evaled Val
            | Delayed Thunk
            deriving (Eq, Show)

type Env = Assoc Variable EnvVal

emptyEnv :: Env
emptyEnv = emptyAssoc

lookupEnv :: Variable -> Env -> Maybe EnvVal
lookupEnv v env = lookupAssocv env

updateEnv :: Variable -> EnvVal -> Env -> Env
updateEnv v envval env = updateAssoc v envval env