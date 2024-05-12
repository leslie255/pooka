use std::{iter::Peekable, rc::Rc};

use crate::{
    source_str::{SourceCharIndices, SourceIndex, SourceStr},
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
enum LexerError {
    InvalidChar(char),
}

impl LexerError {
    fn is_fatal(&self) -> bool {
        match self {
            _ => return false,
        }
    }
}

struct LexerState<'s> {
    path: Rc<str>,
    input: SourceStr<'s>,
    chars: Peekable<SourceCharIndices<'s>>,
}

impl<'s> LexerState<'s> {
    fn new(path: Rc<str>, input: &'s str) -> Self {
        let input = SourceStr::from(input);
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
            (_, c) if c.is_ascii_digit() => todo!("number literals"),
            (start, c) if is_ident_body(c) => {
                let end = take_while(start, &mut self.chars, is_ident_body)?;
                let s = self.input.slice(start..end);
                tokens.push(Token::Ident(s.into()).to_spanned(Span {
                    path: Some(self.path.clone()),
                    range: Some(start..end),
                }));
                Some(Ok(()))
            }
            (_, '\'') => todo!("character literals"),
            (_, '\"') => todo!("string literals"),
            (_, '@') => todo!("macro directives"),
            (start, c) if c.is_unicode_punctuation() => {
                let end = take_while(start, &mut self.chars, char::is_unicode_punctuation)?;
                let s = self.input.slice(start..end);
                let token = parse_puntuation(s);
                tokens.push(token.to_spanned(Span {
                    path: Some(self.path.clone()),
                    range: Some(start..end),
                }));
                Some(Ok(()))
            }
            (i, c) => Some(Err(LexerError::InvalidChar(c).to_spanned(Span {
                path: Some(self.path.clone()),
                range: Some(i..i),
            }))),
        }
    }
}

fn take_while<'s>(
    start: SourceIndex,
    char_indices: &mut Peekable<SourceCharIndices<'s>>,
    mut predicate: impl FnMut(char) -> bool,
) -> Option<SourceIndex> {
    let mut end = start;
    while let Some(&(i, c)) = char_indices.peek() {
        if !predicate(c) {
            break;
        }
        end = i;
        char_indices.next();
    }
    Some(end)
}

fn parse_puntuation(s: &str) -> Token {
    match s {
        "," => Token::Comma,
        "." => Token::Period,
        "=" => Token::Eq,
        ":=" => Token::ColonEq,
        ":" => Token::Colon,
        "::" => Token::ColonColon,
        "*" => Token::Ast,
        "~" => Token::Tilde,
        "&" => Token::Amp,
        "|" => Token::Verbar,
        "^" => Token::Circ,
        ">>" => Token::GtGt,
        "<<" => Token::LtLt,
        ">>=" => Token::GtGtEq,
        "<<=" => Token::LtLtEq,
        "&=" => Token::AmpEq,
        "|=" => Token::VerbarEq,
        "^=" => Token::CircEq,
        "!" => Token::Excl,
        "&&" => Token::AmpAmp,
        "||" => Token::VerbarVerbar,
        "+" => Token::Plus,
        "-" => Token::Minus,
        "/" => Token::Sol,
        "%" => Token::Percnt,
        "+=" => Token::PlusEq,
        "-=" => Token::MinusEq,
        "*=" => Token::AstEq,
        "/=" => Token::SolEq,
        "%=" => Token::PercntEq,
        ">" => Token::Gt,
        "<" => Token::Lt,
        ">=" => Token::GtEq,
        "<=" => Token::LtEq,
        "==" => Token::EqEq,
        "!=" => Token::ExclEq,
        "@" => Token::Commat,
        s => Token::UnreservedPunct(s.into()),
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
