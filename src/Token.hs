module Token where


data Token
    = NameToken String      -- identifier
    | StringToken String    -- string
    | LetToken              -- "="
    | DotToken              -- "."
    | PlusToken             -- "+"
    | EpsToken              -- epsilon: "%" / "Ε" / "ε" / "ϵ"
    | LBraceToken           -- "("
    | RBraceToken           -- ")"
    deriving (Show, Eq)
