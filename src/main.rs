use std::env;

fn main() {
    if env::args().any(|arg| arg == "--version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return;
    }

    if env::args().any(|arg| arg == "--format") {
        let input = env::args()
            .skip(1)
            .filter(|arg| arg != "--format")
            .collect::<Vec<String>>()
            .join(" ");
        return match format(&input) {
            Ok(result) => print!("{}", result),
            Err(err) => {
                eprint!("{}", err);
                std::process::exit(1);
            }
        };
    }

    let input = env::args().skip(1).collect::<Vec<String>>().join(" ");
    return match evaluate(&input) {
        Ok(result) => print!("{}", result),
        Err(err) => {
            eprint!("{}", err);
            std::process::exit(1);
        }
    };
}

fn format(input: &str) -> Result<String, Error> {
    let tokens = tokenize(input)?;
    let (first, following) = parse(&tokens)?;
    return Ok(format!(
        "{}{}",
        first,
        following
            .iter()
            .map(|(op, value)| format!(" {} {}", op, value))
            .collect::<Vec<String>>()
            .join("")
    ));
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Colon => write!(f, ":"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug)]
struct UnexpectedCharacter {
    unexpected: char,
}

#[derive(Clone, Debug)]
struct UnexpectedToken {
    expected: &'static str,
    unexpected: Token,
}

#[derive(Clone, Debug)]
enum Error {
    UnexpectedCharacter(UnexpectedCharacter),
    UnexpectedToken(UnexpectedToken),
    EndOfInput,
}

impl Error {
    fn expected_number_but_found(token: &Token) -> Error {
        Error::UnexpectedToken(UnexpectedToken {
            expected: "number",
            unexpected: token.clone(),
        })
    }

    fn expected_operator_but_found(token: &Token) -> Error {
        Error::UnexpectedToken(UnexpectedToken {
            expected: "operator",
            unexpected: token.clone(),
        })
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedCharacter(unexpected_character) => write!(
                f,
                "error: unexpected character '{}'",
                unexpected_character.unexpected,
            ),
            Error::UnexpectedToken(unexpected_token) => write!(
                f,
                "error: expected {} but found {}",
                unexpected_token.expected, unexpected_token.unexpected
            ),
            Error::EndOfInput => write!(f, "error: unexpected end of input"),
        }
    }
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
            return Err(Error::UnexpectedCharacter(UnexpectedCharacter {
                unexpected: ch,
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

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Subtract => write!(f, "-"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct TimeValue {
    total_minutes: i64,
}

impl TimeValue {
    fn new(hours: i64, minutes: i64) -> Self {
        TimeValue {
            total_minutes: hours * 60 + minutes,
        }
    }

    fn from_hours(hours: i64) -> Self {
        TimeValue::new(hours, 0)
    }

    fn add(&mut self, v: &TimeValue) -> &Self {
        self.total_minutes += v.total_minutes;
        self
    }

    fn subtract(&mut self, v: &TimeValue) -> &Self {
        self.total_minutes -= v.total_minutes;
        self
    }

    fn hours(&self) -> i64 {
        self.total_minutes / 60
    }

    fn minutes(&self) -> i64 {
        self.total_minutes % 60
    }

    fn is_negative(&self) -> bool {
        self.total_minutes < 0
    }
}

impl std::fmt::Display for TimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sign = if self.is_negative() { "-" } else { "" };
        let hours = self.hours().abs();
        let minutes = self.minutes().abs();
        let hour_padding = if hours < 10 { "0" } else { "" };
        let minute_padding = if minutes < 10 { "0" } else { "" };
        write!(
            f,
            "{}{}{}:{}{}",
            sign, hour_padding, hours, minute_padding, minutes,
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

fn parse_value(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<TimeValue, Error> {
    let hours = match it.peek().ok_or(Error::EndOfInput)? {
        Token::Number(n) => {
            it.next();
            Some(n.parse().unwrap())
        }
        _ => None,
    };

    match it.peek() {
        Some(Token::Colon) => it.next(),
        _ if hours.is_some() => return Ok(TimeValue::from_hours(hours.unwrap())),
        Some(t) => return Err(Error::expected_number_but_found(t)),
        None => return Err(Error::EndOfInput),
    };

    match it.peek().ok_or(Error::EndOfInput)? {
        Token::Number(n) => {
            it.next();
            Ok(TimeValue::new(hours.unwrap_or(0), n.parse().unwrap()))
        }
        t => Err(Error::expected_number_but_found(t)),
    }
}

fn parse_operator(
    it: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Operator, Error> {
    let op = match it.peek().ok_or(Error::EndOfInput)? {
        Token::Plus => Operator::Add,
        Token::Minus => Operator::Subtract,
        t => return Err(Error::expected_operator_but_found(t)),
    };
    it.next();
    Ok(op)
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
