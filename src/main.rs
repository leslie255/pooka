#![feature(decl_macro, fn_traits, unboxed_closures)]
#![allow(dead_code)]

use std::rc::Rc;

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
    let src = "mut x : i32 = 0;";
    let tokens = lex::lex(path.clone(), src);
    let mut parser_state = ParserState::new(&tokens, path.clone());
    let parse_result = parse::<Stmt>(&mut parser_state);
    match parse_result {
        Ok(x) => println!("{:?} @ {:?}", x, x.span),
        Err(e) => println!("{:?} @ {:?}", e, e.span),
    }
}
