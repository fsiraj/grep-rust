use std::env;
use std::io;
use std::process;

#[derive(Debug, PartialEq)]
enum Line {
    Start,
    End,
}

#[derive(Debug, PartialEq)]
enum Escape {
    Digit,
    Alphaneumeric,
}

#[derive(Debug, PartialEq)]
enum Quantity {
    Plus,
    Star,
}

#[derive(Debug, PartialEq)]
enum RegexToken {
    Char(char),
    Quantifier {
        token: Box<RegexToken>,
        kind: Quantity,
    },
    Boundary(Line),
    Class(Escape),
    Group {
        group: Vec<RegexToken>,
        negate: bool,
    },
}

fn compile_pattern(pattern: &str) -> Vec<RegexToken> {
    let mut iter = pattern.char_indices();
    let mut tokens = Vec::new();

    while let Some((_, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => {
                    tokens.push(RegexToken::Class(Escape::Digit));
                }
                Some((_, 'w')) => {
                    tokens.push(RegexToken::Class(Escape::Alphaneumeric));
                }
                _ => {}
            },
            '[' => {
                let mut sub_pattern = String::new();
                loop {
                    match iter.next() {
                        Some((_, ']')) => break,
                        Some((_, c_)) => sub_pattern.push(c_),
                        None => panic!("invalid character group"),
                    }
                }
                if sub_pattern.len() > 0 {
                    let (start, negate) =
                        if sub_pattern.len() > 1 && sub_pattern.chars().next().unwrap() == '^' {
                            (1, true)
                        } else {
                            (0, false)
                        };
                    let group = compile_pattern(&sub_pattern[start..]);
                    tokens.push(RegexToken::Group {
                        group: group,
                        negate: negate,
                    });
                }
            }
            '^' | '$' => {
                tokens.push(RegexToken::Boundary(if c == '^' {
                    Line::Start
                } else {
                    Line::End
                }));
            }
            '+' | '*' => {
                let Some(RegexToken::Char(c_)) = tokens.pop() else {
                    panic!("quantifier missing char");
                };
                tokens.push(RegexToken::Quantifier {
                    token: Box::new(RegexToken::Char(c_)),
                    kind: if c == '+' {
                        Quantity::Plus
                    } else {
                        Quantity::Star
                    },
                });
            }
            _ => {
                tokens.push(RegexToken::Char(c));
            }
        }
    }
    tokens
}

fn match_token(c: char, token: &RegexToken) -> bool {
    match token {
        RegexToken::Char(c_) => c == *c_,
        RegexToken::Class(c_class) => match c_class {
            Escape::Digit => c.is_numeric(),
            Escape::Alphaneumeric => c.is_alphanumeric() || c == '_',
        },
        RegexToken::Group { group, negate } => {
            if *negate {
                group.iter().all(|t| !match_token(c, t))
            } else {
                group.iter().any(|t| match_token(c, t))
            }
        }
        RegexToken::Boundary(_) => unreachable!(),
        RegexToken::Quantifier { token: _, kind: _ } => unreachable!(),
    }
}

fn match_quantifier(
    text: &str,
    token: &RegexToken,
    kind: &Quantity,
    pattern: &[RegexToken],
) -> bool {
    let mut iter = text.char_indices().peekable();

    if *kind == Quantity::Plus {
        let Some((_, c_)) = iter.next() else {
            return false;
        };
        if !match_token(c_, token) {
            return false;
        }
    }
    while let Some((i, c_)) = iter.peek() {
        if match_substr(&text[*i..], pattern) {
            return true;
        }
        if match_token(*c_, token) {
            iter.next();
        } else {
            return false;
        }
    }
    match_substr("", pattern)
}

fn match_substr(text: &str, pattern: &[RegexToken]) -> bool {
    // eprintln!("{:?} - {:?}", text, pattern);
    if pattern.len() == 0 {
        return true;
    }
    if pattern[0] == RegexToken::Boundary(Line::End) {
        return text.len() == 0;
    }
    if let RegexToken::Quantifier { token, kind } = &pattern[0] {
        return match_quantifier(text, token.as_ref(), kind, &pattern[1..]);
    }
    if text.len() == 0 {
        return false;
    }
    let c = text.chars().next().unwrap();
    if match_token(c, &pattern[0]) {
        // eprintln!("match, incrementing...");
        let next = c.len_utf8();
        return match_substr(&text[next..], &pattern[1..]);
    }
    false
}

fn match_pattern(text: &str, pattern: &str) -> bool {
    let regex_tokens = compile_pattern(pattern);
    // eprintln!("{:?}", regex_tokens);

    if regex_tokens[0] == RegexToken::Boundary(Line::Start) {
        return match_substr(text, &regex_tokens[1..]);
    }

    for (i, _) in text.char_indices() {
        if match_substr(&text[i..], &regex_tokens) {
            return true;
        }
    }
    false
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        eprintln!("Match");
        process::exit(0)
    } else {
        eprintln!("No match");
        process::exit(1)
    }
}
