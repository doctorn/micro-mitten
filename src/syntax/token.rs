#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Not,
    Plus,
    Minus,
    Mul,
    Div,
    Leq,
    Geq,
    EqEq,
    Neq,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    Ident,
    Let,
    Match,
    If,
    Else,
    Fn,
    Struct,
    Enum,
    U64Type,
    Eq,
    FnArrow,
    MatchArrow,
    Comma,
    Semi,
    Colon,
    Separator,
    LParen,
    RParen,
    LAngle,
    RAngle,
    LCurl,
    RCurl,
    U64Literal,
    Env,
    String,
}

#[derive(Clone, Debug)]
pub struct Token(Kind, Option<String>);

impl From<Kind> for Token {
    fn from(kind: Kind) -> Token {
        Token(kind, None)
    }
}

impl Token {
    pub fn new(kind: Kind, value: &str) -> Token {
        Token(kind, Some(value.to_string()))
    }

    pub fn kind(&self) -> Kind {
        self.0
    }

    pub fn is(&self, kind: Kind) -> bool {
        self.0 == kind
    }

    pub fn value(&self) -> Option<&str> {
        self.1.as_ref().map(|x| &**x)
    }
}
