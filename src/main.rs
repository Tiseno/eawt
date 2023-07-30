use std::env;

fn main() {
    let input = env::args().skip(1).collect::<Vec<String>>().join(" ");
    dbg!(&input);
    let tokens = tokenize(&input);
    dbg!(&tokens);
    let parsed = parse(&tokens);
    dbg!(parsed);
}

#[derive(Clone, Debug)]
enum Token {
    Number(String),
    Colon,
    Plus,
    Minus,
}

impl Token {
    fn name(&self) -> &'static str {
        match &self {
            Token::Number(_) => "Number",
            Token::Colon => "Colon",
            Token::Plus => "Plus",
            Token::Minus => "Minus",
        }
    }
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

#[derive(Clone, Copy, Debug)]
enum Operator {
    Add,
    Subtract,
}

#[derive(Clone, Copy, Debug)]
struct TimeValue {
    hours: u32,
    minutes: u32,
}

impl TimeValue {
    fn from_hours(hours: u32) -> Self {
        TimeValue { hours, minutes: 0 }
    }
}

#[derive(Clone, Copy, Debug)]
enum Parsed {
    Value(TimeValue),
    Operator(Operator),
    EndOfInput,
}

fn parse(tokens: &[Token]) -> Result<Vec<Parsed>, ParseError> {
    let mut it = tokens.iter().peekable();

    let mut parsed: Vec<Parsed> = vec![];
    loop {
        parsed.push(parse_value(&mut it)?);
        match parse_operator(&mut it) {
            Err(ParseError::EndOfInput) => return Ok(parsed),
            Err(err) => return Err(err),
            Ok(p) => parsed.push(p),
        }
    }
}

#[derive(Clone, Debug)]
struct UnexpectedTokenError {
    n: u32,
    expected_token: &'static str,
    unexpected_token: Token,
}

#[derive(Clone, Debug)]
enum ParseError {
    EndOfInput,
    TokenError(UnexpectedTokenError),
}

fn expected_number_error(n: u32, token: &Token) -> ParseError {
    return ParseError::TokenError(UnexpectedTokenError {
        n,
        expected_token: Token::Number(String::new()).name(),
        unexpected_token: token.clone(),
    });
}

fn parse_value(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Parsed, ParseError> {
    let t1 = it.peek().ok_or(ParseError::EndOfInput)?;
    let mut has_parsed = false;
    let hours: u32 = match t1 {
        Token::Number(n) => {
            it.next();
            has_parsed = true;
            n.parse().unwrap()
        }
        _ => 0,
    };

    let t2 = match it.peek() {
        Some(t) => t,
        None => {
            if has_parsed {
                return Ok(Parsed::Value(TimeValue::from_hours(hours)));
            }
            return Err(ParseError::EndOfInput);
        }
    };
    match t2 {
        Token::Colon => it.next(),
        _ => {
            if has_parsed {
                return Ok(Parsed::Value(TimeValue::from_hours(hours)));
            }
            return Err(expected_number_error(1, &t2));
        }
    };

    let t3 = it.peek().ok_or(ParseError::EndOfInput)?;
    match t3 {
        Token::Number(n) => {
            it.next();
            return Ok(Parsed::Value(TimeValue {
                hours,
                minutes: n.parse().unwrap(),
            }));
        }
        _ => return Err(expected_number_error(2, &t3)),
    }
}

fn parse_operator(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Parsed, ParseError> {
    let t = it.peek().ok_or(ParseError::EndOfInput)?;
    let p = match t {
        Token::Plus => Parsed::Operator(Operator::Add),
        Token::Minus => Parsed::Operator(Operator::Subtract),
        _ => return Err(expected_number_error(3, &t)),
    };
    it.next();
    Ok(p)
}
