pub mod lex;

use std::{fmt, fmt::Debug, rc::Rc};

use crate::span::{Span, Spanned, ToSpanned};

pub trait TokenTrait: Sized + Default {
    fn to_token(self) -> Token;
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Token {
    Ident(Rc<str>),
    MacroDir(Rc<str>),
    StrLiteral(Rc<[u8]>),
    IntLiteral(u64),
    FloatLiteral(f64),
    CharLiteral(u32),
    BoolLiteral(bool),
    Mut,
    Struct,
    Union,
    Enum,
    Typealias,
    Type,
    If,
    Else,
    Loop,
    While,
    Return,
    Break,
    Continue,
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,
    UnreservedPunct(Rc<str>),
    Comma,
    Period,
    Eq,
    ColonEq,
    Colon,
    ColonColon,
    Semicolon,
    Ast,
    Tilde,
    Amp,
    Verbar,
    Circ,
    GtGt,
    LtLt,
    GtGtEq,
    LtLtEq,
    AmpEq,
    VerbarEq,
    CircEq,
    Excl,
    AmpAmp,
    VerbarVerbar,
    Plus,
    Minus,
    Sol,
    Percnt,
    PlusEq,
    MinusEq,
    AstEq,
    SolEq,
    PercntEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    EqEq,
    ExclEq,
    Arrow,
    Commat,
}

pub macro Token {
    [mut] => { crate::token::tokens::Mut },
    [struct] => { crate::token::tokens::Struct },
    [union] => { crate::token::tokens::Union },
    [enum] => { crate::token::tokens::Enum },
    [typealias] => { crate::token::tokens::Typealias },
    [type] => { crate::token::tokens::Type },
    [if] => { crate::token::tokens::If },
    [else] => { crate::token::tokens::Else },
    [loop] => { crate::token::tokens::Loop },
    [while] => { crate::token::tokens::While },
    [return] => { crate::token::tokens::Return },
    [break] => { crate::token::tokens::Break },
    [continue] => { crate::token::tokens::Continue },
    [,] => { crate::token::tokens::Comma },
    [.] => { crate::token::tokens::Period },
    [=] => { crate::token::tokens::Eq },
    [:=] => { crate::token::tokens::ColonEq },
    [:] => { crate::token::tokens::Colon },
    [::] => { crate::token::tokens::ColonColon },
    [;] => { crate::token::tokens::Semicolon },
    [*] => { crate::token::tokens::Ast },
    [~] => { crate::token::tokens::Tilde },
    [&] => { crate::token::tokens::Amp },
    [|] => { crate::token::tokens::Verbar },
    [^] => { crate::token::tokens::Circ },
    [>>] => { crate::token::tokens::GtGt },
    [<<] => { crate::token::tokens::LtLt },
    [>>=] => { crate::token::tokens::GtGtEq },
    [<<=] => { crate::token::tokens::LtLtEq },
    [&=] => { crate::token::tokens::AmpEq },
    [|=] => { crate::token::tokens::VerbarEq },
    [^=] => { crate::token::tokens::CircEq },
    [!] => { crate::token::tokens::Excl },
    [&&] => { crate::token::tokens::AmpAmp },
    [||] => { crate::token::tokens::VerbarVerbar },
    [+] => { crate::token::tokens::Plus },
    [-] => { crate::token::tokens::Minus },
    [/] => { crate::token::tokens::Sol },
    [%] => { crate::token::tokens::Percnt },
    [+=] => { crate::token::tokens::PlusEq },
    [-=] => { crate::token::tokens::MinusEq },
    [*=] => { crate::token::tokens::AstEq },
    [/=] => { crate::token::tokens::SolEq },
    [%=] => { crate::token::tokens::PercntEq },
    [>] => { crate::token::tokens::Gt },
    [<] => { crate::token::tokens::Lt },
    [>=] => { crate::token::tokens::GtEq },
    [<=] => { crate::token::tokens::LtEq },
    [==] => { crate::token::tokens::EqEq },
    [!=] => { crate::token::tokens::ExclEq },
    [->] => { crate::token::tokens::Arrow },
    [@] => { crate::token::tokens::Commat },
}

pub mod tokens {
    use super::*;

    macro decl_token_type {
        ($name:ident, $debug:literal $(,)?) => {
            #[derive(Clone, PartialEq, PartialOrd, Default)]
            pub struct $name;
            impl TokenTrait for $name {
                fn to_token(self) -> Token {
                    Token::$name
                }
            }
            impl FnOnce<(Span,)> for $name {
                type Output = Spanned<Token>;
                extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
                    self.to_token().to_spanned(args.0)
                }
            }
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, $debug)
                }
            }
        },
        ($name:ident ( $field:ty ), $default:expr) => {
            #[derive(Clone, PartialEq, PartialOrd)]
            pub struct $name(pub $field);
            impl Default for $name {
                fn default() -> Self {
                    $name($default().into())
                }
            }
            impl TokenTrait for $name {
                fn to_token(self) -> Token {
                    Token::$name(self.0)
                }
            }
            impl FnOnce<(Span,)> for $name {
                type Output = Spanned<Token>;
                extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
                    self.to_token().to_spanned(args.0)
                }
            }
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    Debug::fmt(&self.0, f)
                }
            }
        },
    }

    const fn empty_str() -> &'static str {
        ""
    }

    const fn empty_slice<T>() -> &'static [T] {
        &[]
    }

    decl_token_type!(Ident(Rc<str>), empty_str);
    decl_token_type!(MacroDir(Rc<str>), empty_str);
    decl_token_type!(StrLiteral(Rc<[u8]>), empty_slice);
    decl_token_type!(IntLiteral(u64), u64::default);
    decl_token_type!(FloatLiteral(f64), f64::default);
    decl_token_type!(CharLiteral(u32), u32::default);
    decl_token_type!(BoolLiteral(bool), bool::default);
    decl_token_type!(UnreservedPunct(Rc<str>), empty_str);
    decl_token_type!(Mut, "Token![mut]");
    decl_token_type!(Struct, "Token![struct]");
    decl_token_type!(Union, "Token![union]");
    decl_token_type!(Enum, "Token![enum]");
    decl_token_type!(Typealias, "Token![typealias]");
    decl_token_type!(Type, "Token![type]");
    decl_token_type!(If, "Token![if]");
    decl_token_type!(Else, "Token![else]");
    decl_token_type!(Loop, "Token![loop]");
    decl_token_type!(While, "Token![while]");
    decl_token_type!(Return, "Token![return]");
    decl_token_type!(Break, "Token![break]");
    decl_token_type!(Continue, "Token![continue]");
    decl_token_type!(ParenL, "ParenL");
    decl_token_type!(ParenR, "ParenR");
    decl_token_type!(BracketL, "BracketL");
    decl_token_type!(BracketR, "BracketR");
    decl_token_type!(BraceL, "BraceL");
    decl_token_type!(BraceR, "BraceR");
    decl_token_type!(Comma, "Token![,]");
    decl_token_type!(Period, "Token![.]");
    decl_token_type!(Eq, "Token![=]");
    decl_token_type!(ColonEq, "Token![:=]");
    decl_token_type!(Colon, "Token![:]");
    decl_token_type!(ColonColon, "Token![::]");
    decl_token_type!(Semicolon, "Token![;]");
    decl_token_type!(Ast, "Token![*]");
    decl_token_type!(Tilde, "Token![~]");
    decl_token_type!(Amp, "Token![&]");
    decl_token_type!(Verbar, "Token![|]");
    decl_token_type!(Circ, "Token![^]");
    decl_token_type!(GtGt, "Token![>>]");
    decl_token_type!(LtLt, "Token![<<]");
    decl_token_type!(GtGtEq, "Token![>>=]");
    decl_token_type!(LtLtEq, "Token![<<=]");
    decl_token_type!(AmpEq, "Token![&=]");
    decl_token_type!(VerbarEq, "Token![|=]");
    decl_token_type!(CircEq, "Token![^=]");
    decl_token_type!(Excl, "Token![~]");
    decl_token_type!(AmpAmp, "Token![&&]");
    decl_token_type!(VerbarVerbar, "Token![||]");
    decl_token_type!(Plus, "Token![+]");
    decl_token_type!(Minus, "Token![-]");
    decl_token_type!(Sol, "Token![/]");
    decl_token_type!(Percnt, "Token![%]");
    decl_token_type!(PlusEq, "Token![+=]");
    decl_token_type!(MinusEq, "Token![-=]");
    decl_token_type!(AstEq, "Token![*=]");
    decl_token_type!(SolEq, "Token![/=]");
    decl_token_type!(PercntEq, "Token![%=]");
    decl_token_type!(Gt, "Token![>]");
    decl_token_type!(Lt, "Token![<]");
    decl_token_type!(GtEq, "Token![>=]");
    decl_token_type!(LtEq, "Token![<=]");
    decl_token_type!(EqEq, "Token![==]");
    decl_token_type!(ExclEq, "Token![!=]");
    decl_token_type!(Arrow, "Token![->]");
    decl_token_type!(Commat, "Token![@]");
}
