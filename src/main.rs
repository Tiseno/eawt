use std::env;

fn main() {
    let input = env::args().skip(1).collect::<Vec<String>>().join(" ");
    let tokens = tokenize(&input);
    let parsed = match parse(&tokens) {
        Ok(p) => p,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };
    let result = calculate(&parsed);
    println!("{}", result);
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n) => write!(f, "Number({})", n),
            _ => write!(f, "{}", self.name()),
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut it = input.chars().peekable();

    let mut tokens: Vec<Token> = vec![];

    while let Some(ch) = it.next() {
        if ch == ' ' {
            // Skip whitespace between tokens
        } else if ch.is_ascii_digit() {
            let mut s: String = ch.into();
            while let Some(ch) = it.peek() {
                if ch.is_ascii_digit() {
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
            // TODO handle unexpected tokens
            return tokens;
        }
    }

    tokens
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
    fn new(hours: u32, minutes: u32) -> Self {
        *TimeValue { hours, minutes }.normalize()
    }

    fn from_hours(hours: u32) -> Self {
        TimeValue::new(hours, 0)
    }

    fn normalize(&mut self) -> &Self {
        let total_minutes = self.hours * 60 + self.minutes;
        self.minutes = total_minutes % 60;
        self.hours = (total_minutes - self.minutes) / 60;
        self
    }

    fn add(&mut self, v: &TimeValue) -> &Self {
        self.hours += v.hours;
        self.minutes += v.minutes;
        self.normalize()
    }

    fn subtract(&mut self, v: &TimeValue) -> &Self {
        self.hours -= v.hours;
        self.minutes -= v.minutes;
        self.normalize()
    }
}

impl std::fmt::Display for TimeValue {
    // TODO make this better, will display 8:5 and 0:0 which should be 8:05 and 0:00
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.hours, self.minutes)
    }
}

#[derive(Clone, Copy, Debug)]
enum Parsed {
    Value(TimeValue),
    Operator(Operator),
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
struct UnexpectedToken {
    expected: &'static str,
    unexpected: Token,
}

#[derive(Clone, Debug)]
enum ParseError {
    EndOfInput,
    TokenError(UnexpectedToken),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::EndOfInput => write!(f, "Parse error: unexpected end of input"),
            ParseError::TokenError(unexpected_token) => write!(
                f,
                "Parse error: expected token of variant {} but reached token {}",
                unexpected_token.expected, unexpected_token.unexpected
            ),
        }
    }
}

fn expected_number_error(token: &Token) -> ParseError {
    return ParseError::TokenError(UnexpectedToken {
        expected: Token::Number(String::new()).name(),
        unexpected: token.clone(),
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
            return Err(expected_number_error(t2));
        }
    };

    let t3 = it.peek().ok_or(ParseError::EndOfInput)?;
    match t3 {
        Token::Number(n) => {
            it.next();
            Ok(Parsed::Value(TimeValue::new(hours, n.parse().unwrap())))
        }
        _ => Err(expected_number_error(t3)),
    }
}

fn parse_operator(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Parsed, ParseError> {
    let t = it.peek().ok_or(ParseError::EndOfInput)?;
    let p = match t {
        Token::Plus => Parsed::Operator(Operator::Add),
        Token::Minus => Parsed::Operator(Operator::Subtract),
        _ => return Err(expected_number_error(t)),
    };
    it.next();
    Ok(p)
}

// TODO do this nicer, we know that the parsed list is values separated by operators
// fn calculate(firstValue: TimeValue, terms: &[(Operator, TimeValue)])
fn calculate(parsed: &[Parsed]) -> TimeValue {
    let mut it = parsed.iter().peekable();
    let mut t = match it.next().unwrap() {
        Parsed::Value(t) => *t,
        _ => todo!(),
    };

    while it.peek().is_some() {
        let op = match it.next().unwrap() {
            Parsed::Operator(o) => o,
            _ => todo!(),
        };
        let v = match it.next().unwrap() {
            Parsed::Value(v) => v,
            _ => todo!(),
        };

        match op {
            Operator::Add => t.add(v),
            Operator::Subtract => t.subtract(v),
        };
    }

    t
}
