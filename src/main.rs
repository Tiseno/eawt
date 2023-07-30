use std::env;

fn main() {
    let input = env::args().skip(1).collect::<Vec<String>>().join(" ");
    dbg!(&input);
    let tokens = tokenize(&input);
    dbg!(tokens);
}

#[derive(Debug)]
enum Token {
    Number(String),
    Colon,
    Plus,
    Minus,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut it = input.chars().peekable();

    let mut tokens: Vec<Token> = vec![];

    while let Some(ch) = it.next() {
        if ch.is_digit(10) {
            let mut s: String = ch.into();
            while let Some(ch) = it.peek() {
                if ch.is_digit(10) {
                    s.push(*ch);
                    it.next();
                } else {
                    break;
                }
            }
            tokens.push(Token::Number(s));
        } else if ch == ':' {
            tokens.push(Token::Colon);
        } else if ch == '+' {
            tokens.push(Token::Plus);
        } else if ch == '-' {
            tokens.push(Token::Minus);
        } else {
            return tokens;
        }
    }

    return tokens;
}
