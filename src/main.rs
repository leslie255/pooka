#![feature(decl_macro, fn_traits, unboxed_closures, impl_trait_in_assoc_type)]
#![allow(dead_code)]

use std::rc::Rc;

mod ast;
mod pir;
mod source_str;
mod span;
mod token;
mod utils;

fn main() {
    use crate::ast::parse::*;
    use crate::ast::*;
    use crate::pir::builder::PirBuilderState;
    let path: Rc<str> = "source.pooka".into();
    let src = std::fs::read_to_string(&path[..]).unwrap();
    let tokens = token::lex::lex(path.clone(), &src);
    // dbg!(&tokens);
    let mut parser_state = ParserState::new(&tokens, path.clone());
    while let Some(item) = parse::<AstItem>(&mut parser_state) {
        // match item {
        //     Ok(x) => println!("{:#?} @ {:?}", x, x.1),
        //     Err(e) => println!("{:?} @ {:?}", e, e.1),
        // }
        let mut pir_builder_state = PirBuilderState::new();
        let pir_item = pir::builder::build_item(&mut pir_builder_state, &item.unwrap());
        match pir_item {
            Ok(x) => println!("{:#?}", x),
            Err(e) => println!("{:?} @ {:?}", e, e.1),
        }
    }
}
