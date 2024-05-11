use std::rc::Rc;

use crate::span::{Span, Spanned, ToSpanned};

pub trait TokenTrait: Sized {
    fn to_token() -> Token;
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Token {
    Ident(Rc<str>),
    MacroDir(Rc<str>),
    StrLiteral(Rc<[u8]>),
    IntLiteral(u64),
    FloatLiteral(f64),
    CharLiteral(u32),
    BoolLiteral(bool),
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
    Commat,
}

pub mod tokens {
    use super::*;

    macro decl_token_type($name:ident) {
        pub struct $name;
        impl TokenTrait for $name {
            fn to_token() -> Token {
                Token::$name
            }
        }
        impl FnOnce<(Span,)> for $name {
            type Output = Spanned<Token>;
            extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
                Self::to_token().to_spanned(args.0)
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
    }

    decl_token_type!(Comma);
    decl_token_type!(Period);
    decl_token_type!(Eq);
    decl_token_type!(ColonEq);
    decl_token_type!(Colon);
    decl_token_type!(ColonColon);
    decl_token_type!(Ast);
    decl_token_type!(Tilde);
    decl_token_type!(Amp);
    decl_token_type!(Verbar);
    decl_token_type!(Circ);
    decl_token_type!(GtGt);
    decl_token_type!(LtLt);
    decl_token_type!(GtGtEq);
    decl_token_type!(LtLtEq);
    decl_token_type!(AmpEq);
    decl_token_type!(VerbarEq);
    decl_token_type!(CircEq);
    decl_token_type!(Excl);
    decl_token_type!(AmpAmp);
    decl_token_type!(VerbarVerbar);
    decl_token_type!(Plus);
    decl_token_type!(Minus);
    decl_token_type!(Sol);
    decl_token_type!(Percnt);
    decl_token_type!(PlusEq);
    decl_token_type!(MinusEq);
    decl_token_type!(AstEq);
    decl_token_type!(SolEq);
    decl_token_type!(PercntEq);
    decl_token_type!(Gt);
    decl_token_type!(Lt);
    decl_token_type!(GtEq);
    decl_token_type!(LtEq);
    decl_token_type!(EqEq);
    decl_token_type!(ExclEq);
    decl_token_type!(Commat);

    #[allow(unused)]
    pub macro Token {
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
        [@] => { Commat },
    }
}
