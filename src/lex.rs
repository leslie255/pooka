use std::{iter::Peekable, ops::Range, rc::Rc, str::Chars};

use crate::{
    source_str::{SourceCharIndices, SourceIndex, SourceStr},
    span::{span, Spanned, ToSpanned},
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
            }
            None => break,
        }
    }
    return tokens;
}

#[derive(Debug, Clone)]
enum LexerError {
    UnexpectedEof,
    InvalidChar(char),
    InvalidNumberLiteral,
    InvalidCharLiteral,
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

    fn lex_num_literal(
        &mut self,
        (start, c): (SourceIndex, char),
        tokens: &mut Vec<Spanned<Token>>,
    ) -> Option<Result<(), Spanned<LexerError>>> {
        macro parse_base($i:expr, $parse:expr) {{
            self.chars.next();
            let range = match take_while_(&mut self.chars, is_ident_body) {
                Some(x) => x,
                None => {
                    return Some(Err(
                        LexerError::UnexpectedEof.to_spanned(span!(self.path.clone(), start..$i))
                    ));
                }
            };
            let span = span!(self.path.clone(), start..range.end);
            let s = self.input.slice(range.clone());
            let u = match $parse(&mut s.chars()) {
                Ok(u) => u,
                Err(e) => return Some(Err(e.to_spanned(span))),
            };
            tokens.push(Token::IntLiteral(u).to_spanned(span));
            return Some(Ok(()));
        }}
        if c == '0' {
            match self.chars.peek() {
                Some(&(i, 'b')) => parse_base!(i, parse_int_bin),
                Some(&(i, 'o')) => parse_base!(i, parse_int_oct),
                Some(&(i, 'd')) => parse_base!(i, parse_int_dec),
                Some(&(i, 'x')) => parse_base!(i, parse_int_hex),
                Some(_) | None => {}
            }
        }
        let mut end = take_while(start, &mut self.chars, is_ident_body).unwrap_or(start);
        let span = span!(self.path.clone(), start..end);
        let token = match self.chars.peek() {
            Some(&(_, '.')) => {
                self.chars.next();
                end = take_while(start, &mut self.chars, is_ident_body).unwrap_or(end);
                let s = self.input.slice(start..end);
                let f = if let Ok(f) = s.parse::<f64>() {
                    f
                } else {
                    return Some(Err(LexerError::InvalidNumberLiteral.to_spanned(span)));
                };
                Token::FloatLiteral(f)
            }
            Some(_) | None => {
                let s = self.input.slice(start..end);
                let u = match parse_int_with_exp(s) {
                    Ok(u) => u,
                    Err(e) => return Some(Err(e.to_spanned(span))),
                };
                Token::IntLiteral(u)
            }
        };
        tokens.push(token.to_spanned(span));
        Some(Ok(()))
    }

    #[must_use]
    fn poll(
        &mut self,
        tokens: &mut Vec<Spanned<Token>>,
    ) -> Option<Result<(), Spanned<LexerError>>> {
        match self.chars.next()? {
            (_, c) if c.is_whitespace() => Some(Ok(())),
            x @ (_, c) if c.is_ascii_digit() => self.lex_num_literal(x, tokens),
            (start, '\'') => match self.chars.peek() {
                Some(&(i, '\\')) => {
                    self.chars.next();
                    let u: u32 = match self.chars.peek() {
                        Some((_, 'n')) => {
                            self.chars.next();
                            '\n'.into()
                        }
                        Some((_, 'r')) => {
                            self.chars.next();
                            '\r'.into()
                        }
                        Some((_, 't')) => {
                            self.chars.next();
                            '\t'.into()
                        }
                        Some((_, 'v')) => {
                            self.chars.next();
                            0x0B
                        }
                        Some((_, '\\')) => {
                            self.chars.next();
                            '\\'.into()
                        }
                        Some((_, '0')) => {
                            self.chars.next();
                            0
                        }
                        Some((_, 'x')) => {
                            self.chars.next();
                            todo!()
                        }
                        Some((_, 'u')) => {
                            self.chars.next();
                            todo!()
                        }
                        Some(&(i, _)) => {
                            let e = LexerError::InvalidCharLiteral
                                .to_spanned(span!(self.path.clone(), start..i));
                            return Some(Err(e));
                        }
                        None => {
                            let e = LexerError::UnexpectedEof
                                .to_spanned(span!(self.path.clone(), i..i));
                            return Some(Err(e));
                        }
                    };
                    let token = Token::CharLiteral(u);
                    let end = match self.chars.peek() {
                        Some(&(i, '\'')) => {
                            self.chars.next();
                            i
                        }
                        Some(&(i, _)) => {
                            let e = LexerError::InvalidCharLiteral
                                .to_spanned(span!(self.path.clone(), start..i));
                            return Some(Err(e));
                        }
                        None => {
                            let e = LexerError::UnexpectedEof
                                .to_spanned(span!(self.path.clone(), i..i));
                            return Some(Err(e));
                        }
                    };
                    tokens.push(token.to_spanned(span!(self.path.clone(), start..end)));
                    Some(Ok(()))
                }
                Some(&(i, c)) => {
                    self.chars.next();
                    let token = Token::CharLiteral(c.into());
                    let end = match self.chars.peek() {
                        Some(&(i, '\'')) => {
                            self.chars.next();
                            i
                        }
                        Some(&(i, _)) => {
                            let e = LexerError::InvalidCharLiteral
                                .to_spanned(span!(self.path.clone(), start..i));
                            return Some(Err(e));
                        }
                        None => {
                            let e = LexerError::UnexpectedEof
                                .to_spanned(span!(self.path.clone(), i..i));
                            return Some(Err(e));
                        }
                    };
                    tokens.push(token.to_spanned(span!(self.path.clone(), start..end)));
                    Some(Ok(()))
                }
                None => {
                    return Some(Err(LexerError::UnexpectedEof
                        .to_spanned(span!(self.path.clone(), start..start))));
                }
            },
            (_, '\"') => todo!("string literals"),
            (start, c) if is_ident_body(c) => {
                let end = take_while(start, &mut self.chars, is_ident_body).unwrap_or(start);
                let s = self.input.slice(start..end);
                tokens
                    .push(Token::Ident(s.into()).to_spanned(span!(self.path.clone(), start..end)));
                Some(Ok(()))
            }
            (i, '@') => match self.chars.peek() {
                Some(&(start, c)) if is_ident_body(c) => {
                    self.chars.next();
                    let end = take_while(start, &mut self.chars, is_ident_body).unwrap_or(start);
                    let s = self.input.slice(start..end);
                    tokens.push(
                        Token::MacroDir(s.into()).to_spanned(span!(self.path.clone(), start..end)),
                    );
                    Some(Ok(()))
                }
                Some(_) | None => {
                    tokens.push(Token::Commat.to_spanned(span!(self.path.clone(), i..i)));
                    Some(Ok(()))
                }
            },
            (start, c) if c.is_unicode_punctuation() => {
                let end = take_while(start, &mut self.chars, char::is_unicode_punctuation).unwrap_or(start);
                let s = self.input.slice(start..end);
                let token = parse_puntuation(s);
                tokens.push(token.to_spanned(span!(self.path.clone(), start..end)));
                Some(Ok(()))
            }
            (i, c) => Some(Err(
                LexerError::InvalidChar(c).to_spanned(span!(self.path.clone(), i..i))
            )),
        }
    }
}

fn take_while<'s>(
    start: SourceIndex,
    char_indices: &mut Peekable<SourceCharIndices<'s>>,
    mut predicate: impl FnMut(char) -> bool,
) -> Option<SourceIndex> {
    let &(mut end, c) = char_indices.peek()?;
    if !predicate(c) {
        return Some(start);
    }
    char_indices.next();
    while let Some(&(i, c)) = char_indices.peek() {
        if !predicate(c) {
            return Some(end);
        }
        end = i;
        char_indices.next();
    }
    return Some(end);
}

fn take_while_<'s>(
    char_indices: &mut Peekable<SourceCharIndices<'s>>,
    mut predicate: impl FnMut(char) -> bool,
) -> Option<Range<SourceIndex>> {
    let &(start, c) = char_indices.peek()?;
    let mut end = start;
    if !predicate(c) {
        return None;
    }
    char_indices.next();
    while let Some(&(i, c)) = char_indices.peek() {
        if !predicate(c) {
            return Some(start..end);
        }
        end = i;
        char_indices.next();
    }
    return Some(start..end);
}

fn parse_puntuation(s: &str) -> Token {
    match s {
        "(" => Token::ParenL,
        ")" => Token::ParenR,
        "[" => Token::BracketL,
        "]" => Token::BracketR,
        "{" => Token::BraceL,
        "}" => Token::BraceR,
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

fn parse_int_with_exp(s: &str) -> Result<u64, LexerError> {
    let mut u = 0u64;
    let mut chars = s.chars();
    while let Some(char) = chars.next() {
        u *= 10;
        match char {
            '0' => u += 0,
            '1' => u += 1,
            '2' => u += 2,
            '3' => u += 3,
            '4' => u += 4,
            '5' => u += 5,
            '6' => u += 6,
            '7' => u += 7,
            '8' => u += 8,
            '9' => u += 9,
            'e' => {
                let e = parse_int_dec(&mut chars)?;
                u *= (1..e).fold(1, |acc, _| acc * 10);
                assert!(chars.next().is_none());
                break;
            }
            _ => return Err(LexerError::InvalidNumberLiteral),
        }
    }
    return Ok(u);
}

fn parse_int_bin(chars: &mut Chars) -> Result<u64, LexerError> {
    let mut u = 0u64;
    while let Some(char) = chars.next() {
        u *= 2;
        match char {
            '0' => u += 0,
            '1' => u += 1,
            _ => return Err(LexerError::InvalidNumberLiteral),
        }
    }
    return Ok(u);
}

fn parse_int_oct(chars: &mut Chars) -> Result<u64, LexerError> {
    let mut u = 0u64;
    while let Some(char) = chars.next() {
        u *= 8;
        match char {
            '0' => u += 0,
            '1' => u += 1,
            '2' => u += 2,
            '3' => u += 3,
            '4' => u += 4,
            '5' => u += 5,
            '6' => u += 6,
            '7' => u += 7,
            _ => return Err(LexerError::InvalidNumberLiteral),
        }
    }
    return Ok(u);
}

fn parse_int_dec(chars: &mut Chars) -> Result<u64, LexerError> {
    let mut u = 0u64;
    while let Some(char) = chars.next() {
        u *= 10;
        match char {
            '0' => u += 0,
            '1' => u += 1,
            '2' => u += 2,
            '3' => u += 3,
            '4' => u += 4,
            '5' => u += 5,
            '6' => u += 6,
            '7' => u += 7,
            '8' => u += 8,
            '9' => u += 9,
            _ => return Err(LexerError::InvalidNumberLiteral),
        }
    }
    return Ok(u);
}

fn parse_int_hex(chars: &mut Chars) -> Result<u64, LexerError> {
    let mut u = 0u64;
    while let Some(char) = chars.next() {
        u *= 16;
        u += parse_hex_digit(char).ok_or(LexerError::InvalidNumberLiteral)? as u64;
    }
    return Ok(u);
}

fn parse_hex_digit(c: char) -> Option<u8> {
    match c {
        '0' => Some(0),
        '1' => Some(1),
        '2' => Some(2),
        '3' => Some(3),
        '4' => Some(4),
        '5' => Some(5),
        '6' => Some(6),
        '7' => Some(7),
        '8' => Some(8),
        '9' => Some(9),
        'a' | 'A' => Some(10),
        'b' | 'B' => Some(11),
        'c' | 'C' => Some(12),
        'd' | 'D' => Some(13),
        'e' | 'E' => Some(14),
        'f' | 'F' => Some(15),
        _ => return None,
    }
}
