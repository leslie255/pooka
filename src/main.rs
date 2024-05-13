#![feature(decl_macro, fn_traits, unboxed_closures, try_blocks)]
#![allow(dead_code)]

use std::rc::Rc;

use span::Spanned;

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
    let src = "struct {x: i32, y: i32}";
    let tokens = lex::lex(path.clone(), src);
    for token in &tokens {
        println!("{:?} @ {:?}", token, &token.span);
    }
    let mut parser_state = ParserState::new(&tokens, path.clone());
    let ty: Spanned<Ty> = Parse::parse(&mut parser_state).unwrap();
    dbg!(ty);
}
