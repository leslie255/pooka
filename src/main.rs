#![feature(decl_macro, fn_traits, unboxed_closures, try_blocks)]
#![allow(dead_code)]

use std::rc::Rc;

use span::Spanned;
use token::tokens::{self, Token};

mod ast;
mod lex;
mod parse;
mod source_str;
mod span;
mod token;

fn main() {
    use crate::ast::*;
    use parse::*;
    let path: Rc<str> = "source.pooka".into();
    let src = "[a, b, c, ]";
    let tokens = lex::lex(path.clone(), src);
    let mut parser_state = ParserState::new(&tokens, path.clone());
    let punctuated: Spanned<InBrackets<Punctuated<tokens::Ident, Token![,]>>> =
        Parse::parse(&mut parser_state).unwrap();
    dbg!(punctuated);
}
