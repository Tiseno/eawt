use std::env;

fn main() {
    let input = env::args().skip(1).collect::<Vec<String>>().join(" ");
    match evaluate(&input) {
        Ok(result) => print!("{}", result),
        Err(err) => print!("{}", err),
    };
}

fn evaluate(input: &str) -> Result<TimeValue, Error> {
    let tokens = tokenize(input)?;
    let parsed = parse(&tokens)?;
    Ok(calculate(&parsed))
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

#[derive(Clone, Debug)]
struct UnrecognizedCharacter {
    unrecognized: char,
}

#[derive(Clone, Debug)]
struct UnexpectedToken {
    expected: &'static str,
    unexpected: Token,
}

#[derive(Clone, Debug)]
enum Error {
    UnrecognizedCharacter(UnrecognizedCharacter),
    UnexpectedToken(UnexpectedToken),
    EndOfInput,
}

fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
    let mut it = input.chars().peekable();
    let mut tokens: Vec<Token> = vec![];
    while let Some(ch) = it.next() {
        if ch.is_whitespace() {
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
            return Err(Error::UnrecognizedCharacter(UnrecognizedCharacter {
                unrecognized: ch,
            }));
        }
    }

    Ok(tokens)
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hour_padding = if self.hours < 10 { "0" } else { "" };
        let minute_padding = if self.minutes < 10 { "0" } else { "" };
        write!(
            f,
            "{}{}:{}{}",
            hour_padding, self.hours, minute_padding, self.minutes
        )
    }
}

fn parse(tokens: &[Token]) -> Result<(TimeValue, Vec<(Operator, TimeValue)>), Error> {
    let mut it = tokens.iter().peekable();
    let first = parse_value(&mut it)?;
    let mut following: Vec<(Operator, TimeValue)> = vec![];
    loop {
        if it.peek().is_none() {
            return Ok((first, following));
        }
        let op = parse_operator(&mut it)?;
        let value = parse_value(&mut it)?;
        following.push((op, value));
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnrecognizedCharacter(unrecognized_character) => write!(
                f,
                "Input error: unrecognized character '{}'",
                unrecognized_character.unrecognized,
            ),
            Error::UnexpectedToken(unexpected_token) => write!(
                f,
                "Parse error: expected {} but found {}",
                unexpected_token.expected, unexpected_token.unexpected
            ),
            Error::EndOfInput => write!(f, "Parse error: unexpected end of input"),
        }
    }
}

fn expected_number_error(token: &Token) -> Error {
    Error::UnexpectedToken(UnexpectedToken {
        expected: "Number",
        unexpected: token.clone(),
    })
}

fn expected_operator_error(token: &Token) -> Error {
    Error::UnexpectedToken(UnexpectedToken {
        expected: "Operator",
        unexpected: token.clone(),
    })
}

fn parse_value(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<TimeValue, Error> {
    let t1 = it.peek().ok_or(Error::EndOfInput)?;
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
                return Ok(TimeValue::from_hours(hours));
            }
            return Err(Error::EndOfInput);
        }
    };
    match t2 {
        Token::Colon => it.next(),
        _ => {
            if has_parsed {
                return Ok(TimeValue::from_hours(hours));
            }
            return Err(expected_number_error(t2));
        }
    };

    let t3 = it.peek().ok_or(Error::EndOfInput)?;
    match t3 {
        Token::Number(n) => {
            it.next();
            Ok(TimeValue::new(hours, n.parse().unwrap()))
        }
        _ => Err(expected_number_error(t3)),
    }
}

fn parse_operator(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Operator, Error> {
    let t = it.peek().ok_or(Error::EndOfInput)?;
    let p = match t {
        Token::Plus => Operator::Add,
        Token::Minus => Operator::Subtract,
        _ => return Err(expected_operator_error(t)),
    };
    it.next();
    Ok(p)
}

fn calculate(parsed: &(TimeValue, Vec<(Operator, TimeValue)>)) -> TimeValue {
    parsed
        .1
        .iter()
        .fold(parsed.0, |mut acc, (op, value)| match op {
            Operator::Add => *acc.add(value),
            Operator::Subtract => *acc.subtract(value),
        })
}
