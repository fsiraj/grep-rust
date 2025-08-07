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
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug, PartialEq)]
enum Pattern {
    Char(char),
    Boundary(Line),
    Class(Escape),
    Quantifier {
        pattern: Box<Pattern>,
        kind: Quantity,
    },
    Group {
        group: Vec<Pattern>,
        negate: bool,
    },
}

fn compile_regex(regex: &str) -> Vec<Pattern> {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => {
                    patterns.push(Pattern::Class(Escape::Digit));
                }
                Some((_, 'w')) => {
                    patterns.push(Pattern::Class(Escape::Alphaneumeric));
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
                    let group = compile_regex(&sub_pattern[start..]);
                    patterns.push(Pattern::Group {
                        group: group,
                        negate: negate,
                    });
                }
            }
            '^' if i == 0 => {
                patterns.push(Pattern::Boundary(Line::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(Pattern::Boundary(Line::End));
            }
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(Pattern::Quantifier {
                    pattern: Box::new(pattern),
                    kind: match c {
                        '+' => Quantity::OneOrMore,
                        '*' => Quantity::ZeroOrMore,
                        '?' => Quantity::ZeroOrOne,
                        _ => unreachable!(),
                    },
                });
            }
            _ => {
                patterns.push(Pattern::Char(c));
            }
        }
    }
    patterns
}

fn match_quantifier(
    text: &[char],
    pattern: &Pattern,
    kind: &Quantity,
    next_patterns: &[Pattern],
) -> Option<usize> {
    // try match once, if no match, all three cases return, else ? returns and other two continue
    let mut size = 0;
    match match_substr(text, std::slice::from_ref(pattern)) {
        Some(next_size) => match kind {
            Quantity::ZeroOrOne => return Some(1),
            _ => size += next_size,
        },
        None => match kind {
            Quantity::OneOrMore => return None,
            _ => return Some(0),
        },
    }
    // now keep looping for * and + until rest of pattern matches rest of string
    loop {
        if let Some(_) = match_substr(&text[size..], next_patterns) {
            return Some(size);
        }
        if let Some(next_size) = match_substr(&text[size..], std::slice::from_ref(pattern)) {
            size += next_size;
        } else {
            return Some(size);
        }
    }
}

// returns Some(size) of match, None if no match
fn match_substr(text: &[char], patterns: &[Pattern]) -> Option<usize> {
    eprintln!("{:?} - {:?}", text, patterns);

    if patterns.is_empty() {
        // pattern ended, nothing more to check
        return Some(0);
    }
    if text.is_empty() {
        if let Pattern::Boundary(Line::End) = patterns[0] {
            // end of string matches $
            return Some(0);
        }
        if let Pattern::Quantifier { pattern: _, kind } = &patterns[0] {
            if matches!(kind, Quantity::ZeroOrMore | Quantity::ZeroOrOne) {
                return Some(0);
            }
        }
        // we still have patterns (not $, *, or ?) but text ended
        return None;
    }

    let next_p = &patterns[0];
    let next_c = text[0];

    let result = match next_p {
        Pattern::Char(pattern_c) => (next_c == *pattern_c || *pattern_c == '.').then_some(1),
        Pattern::Class(c_class) => match c_class {
            Escape::Digit => next_c.is_numeric().then_some(1),
            Escape::Alphaneumeric => (next_c.is_alphanumeric() || next_c == '_').then_some(1),
        },
        Pattern::Group { group, negate } => match *negate {
            true => group
                .iter()
                .all(|p| match_substr(text, std::slice::from_ref(p)).is_none())
                .then_some(1),
            false => group
                .iter()
                .any(|p| match_substr(text, std::slice::from_ref(p)).is_some())
                .then_some(1),
        },
        Pattern::Quantifier { pattern, kind } => {
            match_quantifier(text, pattern, kind, &patterns[1..])
        }
        Pattern::Boundary(kind) => match kind {
            Line::End => None,
            Line::Start => unreachable!(),
        },
    };
    let Some(size) = result else {
        return None;
    };

    match match_substr(&text[size..], &patterns[1..]) {
        Some(next_size) => Some(size + next_size),
        None => None,
    }
}

fn match_pattern(text: &str, patterns: &str) -> bool {
    let tokens = compile_regex(patterns);
    let text = text.chars().collect::<Vec<char>>();
    // eprintln!("{:?}", regex_tokens);

    if let Pattern::Boundary(Line::Start) = tokens[0] {
        // no need to test any other starting positions
        let result = match_substr(text.as_slice(), &tokens[1..]);
        eprint!("{:?} - ", result);
        return result.is_some();
    }
    // test all starting positions
    for start in 0..text.len() {
        let result = match_substr(&text[start..], &tokens);
        if result.is_some() {
            eprint!("{:?} - ", result);
            return true;
        }
    }
    // tried all starting positions
    eprint!("{:?} - ", None::<usize>);
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
        eprintln!("Match!");
        process::exit(0)
    } else {
        eprintln!("No match");
        process::exit(1)
    }
}
