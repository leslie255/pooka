#![feature(decl_macro, fn_traits, unboxed_closures, impl_trait_in_assoc_type)]
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
    let src = r#"typealias T = i32;"#;
    let tokens = lex::lex(path.clone(), src);
    let mut parser_state = ParserState::new(&tokens, path.clone());
    let parse_result = parse::<Item>(&mut parser_state);
    match parse_result {
        Ok(x) => println!("{:#?} @ {:?}", x, x.1),
        Err(e) => println!("{:?} @ {:?}", e, e.1),
    }
}
