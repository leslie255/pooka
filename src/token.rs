use std::{rc::Rc, fmt::Debug, fmt};

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
            impl FnMut<(Span,)> for $name {
                extern "rust-call" fn call_mut(&mut self, args: (Span,)) -> Self::Output {
                    self.call_once(args)
                }
            }
            impl Fn<(Span,)> for $name {
                extern "rust-call" fn call(&self, args: (Span,)) -> Self::Output {
                    self.call_once(args)
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
            impl FnMut<(Span,)> for $name {
                extern "rust-call" fn call_mut(&mut self, args: (Span,)) -> Self::Output {
                    self.call_once(args)
                }
            }
            impl Fn<(Span,)> for $name {
                extern "rust-call" fn call(&self, args: (Span,)) -> Self::Output {
                    self.call_once(args)
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

    #[allow(unused)]
    pub macro Token {
        [mut] => { Mut },
        [struct] => { Struct },
        [union] => { Union },
        [enum] => { Enum },
        [typealias] => { Typealias },
        [type] => { Type },
        [if] => { If },
        [loop] => { Loop },
        [while] => { While },
        [return] => { Return },
        [break] => { Break },
        [continue] => { Continue },
        [,] => { Comma },
        [.] => { Period },
        [=] => { Eq },
        [:=] => { ColonEq },
        [:] => { Colon },
        [::] => { ColonColon },
        [*] => { Ast },
        [~] => { Tilde },
        [&] => { Amp },
        [|] => { Verbar },
        [^] => { Circ },
        [>>] => { GtGt },
        [<<] => { LtLt },
        [>>=] => { GtGtEq },
        [<<=] => { LtLtEq },
        [&=] => { AmpEq },
        [|=] => { VerbarEq },
        [^=] => { CircEq },
        [!] => { Excl },
        [&&] => { AmpAmp },
        [||] => { VerbarVerbar },
        [+] => { Plus },
        [-] => { Minus },
        [/] => { Sol },
        [%] => { Percnt },
        [+=] => { PlusEq },
        [-=] => { MinusEq },
        [*=] => { AstEq },
        [/=] => { SolEq },
        [%=] => { PercntEq },
        [>] => { Gt },
        [<] => { Lt },
        [>=] => { GtEq },
        [<=] => { LtEq },
        [==] => { EqEq },
        [!=] => { ExclEq },
        [->] => { Arrow },
        [@] => { Commat },
    }
}
