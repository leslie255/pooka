#![feature(decl_macro, fn_traits, unboxed_closures)]
#![allow(dead_code)]

use std::rc::Rc;

mod span;
mod token;
mod ast;
mod parse;
mod lex;
mod source_str;

fn main() {
    let path: Rc<str> = "source.pooka".into();
    let src = "你好 + %^!$ ident ";
    let tokens = lex::lex(path, src);
    dbg!(tokens);
}
