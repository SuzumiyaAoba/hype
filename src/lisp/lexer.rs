use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Brackets
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    LAngle,     // <
    RAngle,     // >

    // Literals
    Number(f64),
    String(String),
    Bool(bool),

    // Symbols and keywords
    Symbol(String),

    // Special characters
    Colon,      // :
    Arrow,      // ->
    Ampersand,  // &

    // End of file
    Eof,
}

#[derive(Debug, Clone)]
pub struct TokenSpan {
    pub token: Token,
    pub span: Range<usize>,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        let pos = self.pos + offset;
        if pos < self.input.len() {
            Some(self.input[pos])
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.current();
        self.pos += 1;
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // Skip until end of line
        while let Some(ch) = self.current() {
            self.advance();
            if ch == '\n' {
                break;
            }
        }
    }

    fn read_number(&mut self) -> f64 {
        let start = self.pos;

        // Read integer part
        while let Some(ch) = self.current() {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Read decimal part
        if self.current() == Some('.') && self.peek(1).map_or(false, |ch| ch.is_ascii_digit()) {
            self.advance(); // skip '.'
            while let Some(ch) = self.current() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let num_str: String = self.input[start..self.pos].iter().collect();
        num_str.parse().unwrap_or(0.0)
    }

    fn read_string(&mut self) -> String {
        self.advance(); // skip opening quote
        let mut result = String::new();

        while let Some(ch) = self.current() {
            if ch == '"' {
                self.advance(); // skip closing quote
                break;
            } else if ch == '\\' {
                self.advance();
                if let Some(escaped) = self.current() {
                    let ch = match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        _ => escaped,
                    };
                    result.push(ch);
                    self.advance();
                }
            } else {
                result.push(ch);
                self.advance();
            }
        }

        result
    }

    fn read_symbol(&mut self) -> String {
        let start = self.pos;

        while let Some(ch) = self.current() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == '?' || ch == '!' || ch == '/' {
                self.advance();
            } else {
                break;
            }
        }

        self.input[start..self.pos].iter().collect()
    }

    pub fn next_token(&mut self) -> TokenSpan {
        self.skip_whitespace();

        let start = self.pos;

        let token = match self.current() {
            None => Token::Eof,
            Some(';') => {
                self.skip_comment();
                return self.next_token();
            }
            Some('(') => {
                self.advance();
                Token::LParen
            }
            Some(')') => {
                self.advance();
                Token::RParen
            }
            Some('[') => {
                self.advance();
                Token::LBracket
            }
            Some(']') => {
                self.advance();
                Token::RBracket
            }
            Some('{') => {
                self.advance();
                Token::LBrace
            }
            Some('}') => {
                self.advance();
                Token::RBrace
            }
            Some('<') => {
                self.advance();
                Token::LAngle
            }
            Some('>') => {
                self.advance();
                Token::RAngle
            }
            Some(':') => {
                self.advance();
                Token::Colon
            }
            Some('&') => {
                self.advance();
                Token::Ampersand
            }
            Some('-') if self.peek(1) == Some('>') => {
                self.advance();
                self.advance();
                Token::Arrow
            }
            Some('"') => {
                let s = self.read_string();
                Token::String(s)
            }
            Some(ch) if ch.is_ascii_digit() => {
                let num = self.read_number();
                Token::Number(num)
            }
            Some(ch) if ch.is_alphabetic() || ch == '_' => {
                let sym = self.read_symbol();
                match sym.as_str() {
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    _ => Token::Symbol(sym),
                }
            }
            Some(ch) => {
                self.advance();
                // For now, treat unknown characters as symbols
                Token::Symbol(ch.to_string())
            }
        };

        TokenSpan {
            token,
            span: start..self.pos,
        }
    }

    pub fn tokenize(&mut self) -> Vec<TokenSpan> {
        let mut tokens = Vec::new();
        loop {
            let token_span = self.next_token();
            let is_eof = matches!(token_span.token, Token::Eof);
            tokens.push(token_span);
            if is_eof {
                break;
            }
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_brackets() {
        let mut lexer = Lexer::new("( ) [ ] { } < >");
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LBrace,
                Token::RBrace,
                Token::LAngle,
                Token::RAngle,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("123 45.67 0.5");
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Number(123.0),
                Token::Number(45.67),
                Token::Number(0.5),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new(r#""hello" "world""#);
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::String("hello".to_string()),
                Token::String("world".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_symbols() {
        let mut lexer = Lexer::new("+ defn let match");
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Symbol("+".to_string()),
                Token::Symbol("defn".to_string()),
                Token::Symbol("let".to_string()),
                Token::Symbol("match".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_simple_sexp() {
        let mut lexer = Lexer::new("(+ 1 2)");
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Symbol("+".to_string()),
                Token::Number(1.0),
                Token::Number(2.0),
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("; This is a comment\n(+ 1 2)");
        let tokens: Vec<Token> = lexer.tokenize().into_iter().map(|t| t.token).collect();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Symbol("+".to_string()),
                Token::Number(1.0),
                Token::Number(2.0),
                Token::RParen,
                Token::Eof,
            ]
        );
    }
}
