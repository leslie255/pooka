use std::{iter::Peekable, rc::Rc, str::CharIndices};

use crate::{
    span::{Span, Spanned, ToSpanned},
    token::Token,
};

pub fn lex(path: Rc<str>, input: &str) -> Vec<Spanned<Token>> {
    let mut tokens = Vec::<Spanned<Token>>::new();
    let mut lexer = LexerState::new(path, input);
    loop {
        match lexer.poll(&mut tokens) {
            Some(Ok(())) => (),
            Some(Err(e)) => {
                println!("lexer error: {:?}", e);
                if e.is_fatal() {
                    std::process::exit(1);
                }
            }
            None => break,
        }
    }
    return tokens;
}

#[derive(Debug, Clone)]
enum LexerError {}

impl LexerError {
    fn is_fatal(&self) -> bool {
        match self {
            _ => return false,
        }
    }
}

struct LexerState<'s> {
    path: Rc<str>,
    input: &'s str,
    chars: Peekable<CharIndices<'s>>,
}

impl<'s> LexerState<'s> {
    fn new(path: Rc<str>, input: &'s str) -> Self {
        Self {
            path,
            input,
            chars: input.char_indices().peekable(),
        }
    }

    #[must_use]
    fn poll(
        &mut self,
        tokens: &mut Vec<Spanned<Token>>,
    ) -> Option<Result<(), Spanned<LexerError>>> {
        match self.chars.next()? {
            (_, c) if c.is_ascii_digit() => todo!("int/float literals"),
            (start, c) if is_ident_body(c) => {
                let mut end = start;
                while let Some(&(i, c)) = self.chars.peek() {
                    end = i;
                    if !is_ident_body(c) {
                        break;
                    }
                    self.chars.next();
                }
                let s = &self.input[start..=end];
                tokens.push(Token::Ident(s.into()).to_spanned(Span {
                    path: Some(self.path.clone()),
                    start,
                    end,
                }));
                Some(Ok(()))
            }
            (_, '\'') => todo!("character literals"),
            (_, '\"') => todo!("string literals"),
            (_, '@') => todo!("macro directives"),
            (_, c) if c.is_unicode_punctuation() => todo!("punctuations"),
            _ => todo!("punctuations"),
        }
    }
}

fn is_ident_body(c: char) -> bool {
    (c.is_alphanumeric())
        || c == '_'
        || c == '\''
        || unic_emoji_char::is_emoji(c)
        || unic_emoji_char::is_emoji_component(c)
}

trait CharExtensions {
    fn is_unicode_punctuation(self) -> bool;
}

impl CharExtensions for char {
    fn is_unicode_punctuation(self) -> bool {
        self.is_ascii_punctuation()
            || unicode_blocks::GENERAL_PUNCTUATION.contains(self)
            || unicode_blocks::SUPPLEMENTAL_PUNCTUATION.contains(self)
            || unicode_blocks::MATHEMATICAL_OPERATORS.contains(self)
            || unicode_blocks::SUPPLEMENTAL_MATHEMATICAL_OPERATORS.contains(self)
    }
}
