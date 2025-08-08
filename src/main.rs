use std::env;
use std::fs;
use std::io;
use std::process;

#[derive(Debug, PartialEq)]
enum Meta {
    Digit,
    Alphanumeric,
    Start,
    End,
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug, PartialEq)]
enum Pattern {
    Char(char),
    Class(Meta),
    Bound(Meta),
    Group { group: Regex, negate: bool },
    Alternates { regexes: Alternates },
    Quantifier { pattern: Box<Pattern>, kind: Meta },
}

type Regex = Vec<Pattern>;
type Alternates = Vec<Regex>;

// Returns an AST form of the regex
fn compile_regex(regex: &str) -> Regex {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => {
                    patterns.push(Pattern::Class(Meta::Digit));
                }
                Some((_, 'w')) => {
                    patterns.push(Pattern::Class(Meta::Alphanumeric));
                }
                _ => {}
            },
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
                    let group = compile_regex(&subpattern[start..]);
                    patterns.push(Pattern::Group { group, negate });
                }
            }
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
                    let alternates = alternates.iter().map(|r| compile_regex(r)).collect();
                    patterns.push(Pattern::Alternates {
                        regexes: alternates,
                    });
                }
            }
            '^' if i == 0 => {
                patterns.push(Pattern::Bound(Meta::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(Pattern::Bound(Meta::End));
            }
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(Pattern::Quantifier {
                    pattern: Box::new(pattern),
                    kind: match c {
                        '+' => Meta::OneOrMore,
                        '*' => Meta::ZeroOrMore,
                        '?' => Meta::ZeroOrOne,
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

/// return Some(size) of match where size is shortest match of quantifier, None otherwise
fn match_quantifier(
    text: &[char],
    pattern: &Pattern,
    kind: &Meta,
    next_patterns: &[Pattern],
) -> Option<usize> {
    let pattern = std::slice::from_ref(pattern);
    // at least one match for +
    let mut size = match kind {
        Meta::OneOrMore => match_substr(text, pattern)?,
        _ => 0,
    };
    loop {
        // try 0 matches for + and *, >1 for +
        if text[size..].is_empty() && match_substr(&text[size..], next_patterns).is_some() {
            return Some(size);
        }
        if !next_patterns.is_empty() && match_substr(&text[size..], next_patterns).is_some() {
            return Some(size);
        }
        match kind {
            Meta::ZeroOrOne => {
                // don't loop for ?
                return match_substr(&text[size..], pattern);
            }
            _ => {
                // keep moving forward for + and *
                if let Some(next_size) = match_substr(&text[size..], pattern) {
                    size += next_size;
                } else {
                    eprintln!("alpha");
                    return None;
                }
            }
        }
    }
}

// returns Some(size) of match, None if no match
fn match_substr(text: &[char], patterns: &[Pattern]) -> Option<usize> {
    // eprintln!(
    //     "{:?} - {:?} ({:?})",
    //     text.iter().collect::<String>(),
    //     patterns.get(0).unwrap_or(&Pattern::Char('~')),
    //     patterns.len()
    // );

    if patterns.is_empty() {
        // pattern ended, nothing more to check
        return Some(0);
    }
    if text.is_empty() {
        if let Pattern::Bound(Meta::End) = patterns[0] {
            // end of string matches $
            return Some(0);
        }
        if let Pattern::Quantifier { pattern: _, kind } = &patterns[0] {
            if matches!(kind, Meta::ZeroOrMore | Meta::ZeroOrOne) {
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
            Meta::Digit => next_c.is_numeric().then_some(1),
            Meta::Alphanumeric => (next_c.is_alphanumeric() || next_c == '_').then_some(1),
            _ => unreachable!(),
        },
        Pattern::Bound(kind) => match kind {
            Meta::End => None,
            Meta::Start => unreachable!(),
            _ => unreachable!(),
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
        Pattern::Alternates {
            regexes: alternates,
        } => alternates.iter().find_map(|r| match_substr(text, r)),
        Pattern::Quantifier { pattern, kind } => {
            match_quantifier(text, pattern, kind, &patterns[1..])
        }
    };
    let size = result?;

    Some(size + match_substr(&text[size..], &patterns[1..])?)
}

fn match_pattern(text: &str, regex: &str) -> Option<usize> {
    // eprintln!("{:?} ({:?})", text, text.len());
    // eprintln!("{:?} ({:?})", regex, regex.len());
    let text = text.chars().collect::<Vec<char>>();
    let regex = compile_regex(regex);
    // eprintln!("\n{:?} ({:?})", regex, regex.len());

    if let Pattern::Bound(Meta::Start) = regex[0] {
        // no need to test any other starting positions
        return match_substr(text.as_slice(), &regex[1..]);
    }
    // test all starting positions
    for start in 0..text.len() {
        if let Some(size) = match_substr(&text[start..], &regex) {
            return Some(size);
        }
    }
    // tried all starting positions
    None
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 3 || args[1] != "-E" {
        println!("Usage: grep.sh -E <pattern> <file>");
        println!("       echo <input> | grep.sh -E <pattern> <file>");
        process::exit(1);
    }

    let pattern = &args[2];
    let mut input_line = String::new();

    if args.len() > 3 {
        let filepath = &args[3];
        input_line = fs::read_to_string(filepath).expect("unable to open file");
        // eprintln!("{:?}", input_line);
    } else {
        io::stdin().read_line(&mut input_line).unwrap();
    }
    input_line = input_line.trim().to_string();

    if match_pattern(&input_line, &pattern).is_some() {
        println!("{}", input_line);
        process::exit(0)
    } else {
        process::exit(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn matches(text: &str, regex: &str) -> bool {
        match_pattern(text, regex).is_some()
    }

    #[test]
    fn test_literal_character() {
        assert!(matches("dog", "d"));
        assert!(!matches("dog", "f"));
    }

    #[test]
    fn test_digits() {
        assert!(matches("123", r"\d"));
        assert!(!matches("apple", r"\d"));
    }

    #[test]
    fn test_alphanumeric() {
        assert!(matches("pear", r"\w"));
        assert!(matches("STRAWBERRY", r"\w"));
        assert!(matches("129", r"\w"));
        assert!(matches("+#-_-%÷", r"\w"));
        assert!(!matches("%#÷=×+", r"\w"));
    }

    #[test]
    fn test_positive_character_groups() {
        assert!(matches("l", "[apple]"));
        assert!(!matches("banana", "[cdefgh]"));
        assert!(!matches("[]", "[pineapple]"));
    }

    #[test]
    fn test_negative_character_groups() {
        assert!(matches("apple", "[^xyz]"));
        assert!(matches("apple", "[^abc]"));
        assert!(!matches("banana", "[^anb]"));
        assert!(matches("orange", "[^opq]"));
    }

    #[test]
    fn test_combining_character_classes() {
        assert!(matches("sally has 3 apples", r"\d apple"));
        assert!(!matches("sally has 1 orange", r"\d apple"));
        assert!(matches("sally has 124 apples", r"\d\d\d apples"));
        assert!(!matches("sally has 12 apples", r"\d\\d\\d apples"));
        assert!(matches("sally has 3 dogs", r"\d \w\w\ws"));
        assert!(matches("sally has 4 dogs", r"\d \w\w\ws"));
        assert!(!matches("sally has 1 dog", r"\d \w\w\ws"));
    }

    #[test]
    fn test_start_of_string_anchor() {
        assert!(matches("log", "^log"));
        assert!(!matches("slog", "^log"));
    }

    #[test]
    fn test_end_of_string_anchor() {
        assert!(matches("cat", "cat$"));
        assert!(!matches("cats", "cat$"));
    }

    #[test]
    fn test_one_or_more_times() {
        assert!(matches("cat", "ca+t"));
        assert!(matches("caaats", "ca+at"));
        assert!(matches("a", "a+"));
        assert!(matches("aa", "a+a"));
        assert!(matches("aa", "aa+"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
    }

    #[test]
    fn test_zero_or_more_times() {
        assert!(matches("cat", "ca*t"));
        assert!(matches("caaats", "ca*t"));
        assert!(matches("a", "a*"));
        assert!(matches("aa", "a*a"));
        assert!(matches("aa", "aa*"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
    }

    #[test]
    fn test_zero_or_one_times() {
        assert!(matches("cat", "ca?t"));
        assert!(matches("act", "ca?t"));
        assert!(!matches("dog", "ca?t"));
        assert!(!matches("cag", "ca?t"));
    }

    #[test]
    fn test_wildcard() {
        assert!(matches("cat", "c.t"));
        assert!(!matches("car", "c.t"));
        assert!(matches("goøö0Ogol", "g.+gol"));
        assert!(!matches("gol", "g.+gol"));
    }

    #[test]
    fn test_alternation() {
        assert!(matches("a cat", "a (cat|dog)"));
        assert!(!matches("a cow", "a (cat|dog)"));
        assert!(matches(
            "I see 1 cat, 2 dogs and 3 cows",
            "^I see (\\d (cat|dog|cow)s?(, | and )?)+$"
        ));
        assert!(!matches(
            "I see 1 cat, 2 dogs and 3 cows",
            "^I see (\\d (cat|dog|cow)(, | and )?)+$"
        ));
    }
}
