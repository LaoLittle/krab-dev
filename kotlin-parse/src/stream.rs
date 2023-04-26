use self::Token::*;
use kotlin_lexer::TokenKind as LxTkKind;
use kotlin_lexer::{Base, Cursor, LiteralKind};
use kotlin_span::Span;

pub struct TokenStream<'a> {
    source: &'a str,
    cursor: Cursor<'a>,
    lookahead: Option<kotlin_lexer::Token>,
    prev_pos: usize,
    pos: usize,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tk = self.advance_token();

        if matches!(tk, Eof) {
            None
        } else {
            Some(tk)
        }
    }
}

impl<'a> TokenStream<'a> {
    #[inline]
    pub fn new(input: &'a str) -> Self {
        Self {
            source: input,
            cursor: Cursor::new(input),
            lookahead: None,
            prev_pos: 0,
            pos: 0,
        }
    }

    pub fn advance_token(&mut self) -> Token {
        macro_rules! op_match {
            ($($lxkind:ident => $tk:ident),+; $else:ident) => {
                match self.peek_next().kind {
                    $(
                    LxTkKind::$lxkind => {
                        self.bump();
                        Token::$tk
                    },
                    )+
                    _ => $else
                }
            };
        }

        let mut tk = self.next_cursor_token();

        loop {
            break match tk.kind {
                LxTkKind::NewLine => NewLine,
                LxTkKind::At => At,
                LxTkKind::Semi => Semi,
                LxTkKind::Comma => Comma,
                LxTkKind::Colon => Colon,
                LxTkKind::OpenParen => OpenParen,
                LxTkKind::CloseParen => CloseParen,
                LxTkKind::OpenBracket => OpenBracket,
                LxTkKind::CloseBracket => CloseBracket,
                LxTkKind::OpenBrace => OpenBrace,
                LxTkKind::CloseBrace => CloseBrace,
                LxTkKind::Literal { kind, suffix_start } => {
                    match kind {
                        LiteralKind::Int { base, empty_int } => {
                            if empty_int {
                                Unknown
                            } else {
                                let mut suffix = None::<IntSuffix>;
                                let mut intpos = self.prev_pos();
                                let mut intlen = suffix_start;
                                if !matches!(base, Base::Decimal) {
                                    intpos += 2;
                                    intlen -= 2;
                                }
                                // 0o 32 u
                                let suff_start = self.prev_pos() + suffix_start as usize;
                                if suff_start != self.pos() {
                                    let span = Span::new_with_end(
                                        self.prev_pos() + suffix_start as usize,
                                        self.pos(),
                                    );
                                    suffix = Some(IntSuffix::from_source(self.source(), span));
                                }
                                let intsp = Span::new(intpos, intlen);
                                let intstr = intsp.str_slice(self.source());
                                let int = u128::from_str_radix(intstr, base as u32);

                                int.map(|i| Literal(Literal::Integer { int: i, suffix }))
                                    .unwrap_or(Unknown)
                            }
                        }
                        _ => unimplemented!(),
                    }
                }
                LxTkKind::Eq => match self.peek_next().kind {
                    LxTkKind::Eq => {
                        self.bump();
                        match self.peek_next().kind {
                            LxTkKind::Eq => ExactEq,
                            _ => Eq,
                        }
                    }
                    _ => Assign,
                },
                LxTkKind::Gt => op_match!(Eq => GreaterEq; Greater),
                LxTkKind::Lt => op_match!(Eq => LessEq; Less),
                // kotlin have no bitand operator
                LxTkKind::And => op_match!(And => And; Unknown),
                // kotlin have no bitor operator
                LxTkKind::Or => op_match!(Or => Or; Unknown),
                LxTkKind::Plus => op_match!(
                    Plus => Inc,
                    Eq => PlusAssign;
                    Plus
                ),
                LxTkKind::Minus => op_match!(
                    Minus => Dec,
                    Eq => MinusAssign,
                    Gt => Arrow;
                    Minus
                ),
                LxTkKind::Star => op_match!(
                    Eq => MulAssign;
                    Mul
                ),
                LxTkKind::Slash => op_match!(
                    Eq => DivAssign;
                    Div
                ),
                LxTkKind::Percent => op_match!(
                    Eq => RemAssign;
                    Rem
                ),
                LxTkKind::Bang => match self.peek_next().kind {
                    LxTkKind::Eq => {
                        self.bump();

                        match self.peek_next().kind {
                            LxTkKind::Eq => {
                                self.bump();
                                NotExactEq
                            }
                            _ => NotEq,
                        }
                    }
                    LxTkKind::Ident => {
                        let Self {
                            source,
                            prev_pos,
                            pos,
                            ..
                        } = *self;
                        let ident = &source[prev_pos..pos];
                        let kw = keyword_or_ident(ident);

                        match kw {
                            Is => {
                                self.bump();
                                NotIs
                            }
                            In => {
                                self.bump();
                                NotIn
                            }
                            _ => Not,
                        }
                    }
                    _ => Not,
                },
                LxTkKind::Question => op_match!(
                    Colon => Elvis;
                    Question
                ),
                LxTkKind::Dot => op_match!(
                    Dot => DotDot;
                    Dot
                ),
                LxTkKind::Ident => {
                    let Self {
                        source,
                        prev_pos,
                        pos,
                        ..
                    } = *self;
                    let ident = &source[prev_pos..pos];

                    let mut kw = keyword_or_ident(ident);

                    if let As = kw {
                        if let LxTkKind::Question = self.peek_next().kind {
                            self.bump();
                            kw = AsSafe;
                        }
                    }

                    kw
                }
                LxTkKind::Eof => Eof,
                LxTkKind::Whitespace
                | LxTkKind::LineComment { .. }
                | LxTkKind::BlockComment { .. } => {
                    tk = self.next_cursor_token();
                    continue;
                }
                _ => Unknown,
            };
        }
    }

    pub fn peek_next(&mut self) -> kotlin_lexer::Token {
        let tk = self.next_cursor_token();
        self.lookahead = Some(tk.clone());

        tk
    }

    pub fn next_cursor_token(&mut self) -> kotlin_lexer::Token {
        if let Some(t) = self.lookahead.take() {
            return t;
        }

        let tk = self.cursor.advance_token();

        if tk.len != 0 {
            self.prev_pos = self.pos;
            self.pos += tk.len as usize;
        }

        tk
    }

    #[inline]
    pub const fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    pub const fn prev_pos(&self) -> usize {
        self.prev_pos
    }

    pub const fn source(&self) -> &str {
        self.source
    }

    pub fn bump(&mut self) {
        self.next_cursor_token();
    }
}

fn keyword_or_ident(s: &str) -> Token {
    match s {
        "abstract" => Abstract,
        "if" => If,
        "else" => Else,
        "while" => While,
        "for" => For,
        "do" => Do,
        "constructor" => Constructor,
        "class" => Class,
        "fun" => Fun,
        "external" => External,
        "operator" => Operator,
        "infix" => Infix,
        "suspend" => Suspend,
        "continue" => Continue,
        "break" => Break,
        "return" => Return,
        "throw" => Throw,
        "try" => Try,
        "catch" => Catch,
        "finally" => Finally,
        "package" => Package,
        "import" => Import,
        "this" => This,
        "super" => Super,
        "in" => In,
        "is" => Is,
        "as" => As,
        "object" => Object,
        "true" => True,
        "false" => False,
        "val" => Val,
        "var" => Var,
        "vararg" => Vararg,
        "typealias" => TypeAlias,
        "when" => When,
        "null" => Null,
        _ => Ident,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Literal(Literal),
    Abstract,
    /// .
    Dot,
    /// ..
    DotDot,
    /// ,
    Comma,
    /// :
    Colon,
    /// >
    Greater,
    /// >=
    GreaterEq,
    /// <
    Less,
    /// <=
    LessEq,
    /// \n
    NewLine,
    /// ;
    Semi,
    /// @
    At,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// &&
    And,
    /// ||
    Or,
    /// !
    Not,
    /// ++
    Inc,
    /// --
    Dec,
    /// \+
    Plus,
    /// +=
    PlusAssign,
    /// \-
    Minus,
    /// -=
    MinusAssign,
    /// \*
    Mul,
    /// *=
    MulAssign,
    /// /
    Div,
    /// /=
    DivAssign,
    /// %
    Rem,
    /// %=
    RemAssign,
    /// ==
    Eq,
    /// ===
    ExactEq,
    /// !=
    NotEq,
    /// !==
    NotExactEq,
    /// =
    Assign,
    /// ->
    Arrow,
    /// ?:
    Elvis,
    /// ?
    Question,
    /// package
    Package,
    /// import
    Import,
    /// ident
    Ident,
    /// if
    If,
    /// else
    Else,
    /// while
    While,
    /// for
    For,
    /// do
    Do,
    /// fun
    Fun,
    /// suspend
    Suspend,
    /// external
    External,
    /// infix
    Infix,
    /// operator
    Operator,
    /// class
    Class,
    /// object
    Object,
    /// constructor
    Constructor,
    /// as
    As,
    /// as?
    AsSafe,
    /// true
    True,
    /// false
    False,
    /// in
    In,
    /// !in
    NotIn,
    /// is
    Is,
    /// !is
    NotIs,
    /// this
    This,
    /// super
    Super,
    /// throw
    Throw,
    /// try
    Try,
    /// catch
    Catch,
    /// finally
    Finally,
    /// break
    Break,
    /// continue
    Continue,
    /// return
    Return,
    /// typealias
    TypeAlias,
    /// when
    When,
    /// val
    Val,
    /// var
    Var,
    /// vararg
    Vararg,
    /// null
    Null,
    /// end of file
    Eof,
    /// unknown token
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer {
        int: u128,
        suffix: Option<IntSuffix>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntSuffix {
    // 1L
    Long,
    // 1u
    Unsigned,
    Unknown(Span),
}

impl IntSuffix {
    pub fn from_source(source: &str, span: Span) -> Self {
        let s = span.str_slice(source);
        match s {
            "L" => Self::Long,
            "u" => Self::Unsigned,
            _ => Self::Unknown(span),
        }
    }
}
