#![feature(decl_macro, fn_traits, unboxed_closures, impl_trait_in_assoc_type)]
#![allow(dead_code)]

use std::rc::Rc;

mod ast;
mod hir;
mod source_str;
mod span;
mod token;
mod utils;

fn main() {
    use crate::ast::parse::*;
    use crate::ast::*;
    use crate::hir::builder::HirBuilderState;
    let path: Rc<str> = "source.pooka".into();
    let src = std::fs::read_to_string(&path[..]).unwrap();
    let tokens = token::lex::lex(path.clone(), &src);
    // dbg!(&tokens);
    let mut parser_state = ParserState::new(&tokens, path.clone());
    while let Some(item) = parse::<AstItem>(&mut parser_state) {
        // dbg!(&item);
        let mut pir_builder_state = HirBuilderState::new();
        let pir_item = hir::builder::build_item(&mut pir_builder_state, &item.unwrap());
        match pir_item {
            Ok(x) => println!("{:#?}", x),
            Err(e) => println!("{:?} @ {:?}", e, e.1),
        }
    }
}
