use std::rc::Rc;

// Represent special characters in regex syntax
#[derive(Debug)]
pub enum Meta {
    Digit,
    Alphanumeric,
    Wildcard,
    Start,
    End,
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

// Represent the Abstract Syntax Tree
#[derive(Debug)]
pub enum Token {
    Char(char),
    Class(Meta),
    Bound(Meta),
    CharGroup {
        chars: Vec<Token>, // only chars
        negate: bool,
    },
    CaptureGroup {
        alternates: Vec<Vec<Token>>,
        index: usize,
    },
    Quantifier {
        token: Rc<Token>,
        kind: Meta,
    },
    BackRef(usize),
}

// Parses a string into an AST
pub fn string_to_ast(regex: &str, capture_index: &mut usize) -> Vec<Token> {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
            // Could be character class or backreference
            '\\' => match iter.next() {
                Some((_, 'd')) => patterns.push(Token::Class(Meta::Digit)),
                Some((_, 'w')) => patterns.push(Token::Class(Meta::Alphanumeric)),
                Some((_, other)) if other.is_numeric() => {
                    let index = other.to_digit(10).unwrap() as usize;
                    if index == 0 {
                        panic!("backref are 1-indexed, upto 9")
                    }
                    patterns.push(Token::BackRef(index))
                }
                _ => {}
            },
            // Matches any character
            '.' => {
                patterns.push(Token::Class(Meta::Wildcard));
            }
            // Starts a character group, should only contain literal characters inside
            '[' => {
                let mut subpattern = String::new();
                loop {
                    match iter.next() {
                        Some((_, ']')) => break,
                        Some((_, other)) => subpattern.push(other),
                        None => panic!("unclosed [ character group"),
                    }
                }
                if !subpattern.is_empty() {
                    let (start, negate) = if subpattern.starts_with('^') {
                        (1, true)
                    } else {
                        (0, false)
                    };
                    let group = string_to_ast(&subpattern[start..], capture_index);
                    patterns.push(Token::CharGroup {
                        chars: group,
                        negate,
                    });
                }
            }
            // Starts a capture group, can include any other tokens including nested capture
            // groups which are parsed recursively
            '(' => {
                let mut depth = 0;
                let mut subpattern = String::new();
                let mut alternates = Vec::new();
                loop {
                    match iter.next() {
                        Some((_, ')')) => {
                            if depth == 0 {
                                alternates.push(subpattern);
                                break;
                            }
                            depth -= 1;
                            subpattern.push(')');
                        }
                        Some((_, '(')) => {
                            depth += 1;
                            subpattern.push('(');
                        }
                        Some((_, '|')) => {
                            if depth == 0 {
                                alternates.push(subpattern);
                                subpattern = String::new();
                                continue;
                            }
                            subpattern.push('|');
                        }
                        Some((_, other)) => subpattern.push(other),
                        None => panic!("unclosed ( alternates group"),
                    }
                }
                if !alternates.is_empty() {
                    let this_index = *capture_index;
                    *capture_index += 1;
                    let alternates = alternates
                        .iter()
                        .map(|regex| string_to_ast(regex, capture_index))
                        .collect();
                    patterns.push(Token::CaptureGroup {
                        alternates,
                        index: this_index,
                    });
                }
            }
            // Matches start of string, causes modification to input string
            '^' if i == 0 => {
                patterns.push(Token::Bound(Meta::Start));
            }
            // Matches end of string, causes modification to input string
            '$' if i == regex.len() - 1 => {
                patterns.push(Token::Bound(Meta::End));
            }
            // Quantifiers can be applied to any other token, but not to itself
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(Token::Quantifier {
                    token: Rc::new(pattern),
                    kind: match c {
                        '+' => Meta::OneOrMore,
                        '*' => Meta::ZeroOrMore,
                        '?' => Meta::ZeroOrOne,
                        _ => unreachable!(),
                    },
                });
            }
            // Literal characters are trivial
            _ => {
                patterns.push(Token::Char(c));
            }
        }
    }
    patterns
}
