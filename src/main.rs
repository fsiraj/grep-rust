use clap::Parser;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path;
use std::process;
use std::rc::Rc;
use walkdir::WalkDir;

// Core matching logic

#[derive(Debug)]
enum Meta {
    Digit,
    Alphanumeric,
    Wildcard,
    Start,
    End,
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug)]
enum Token {
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
fn string_to_ast(regex: &str, capture_index: &mut usize) -> Vec<Token> {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
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
            '.' => {
                patterns.push(Token::Class(Meta::Wildcard));
            }
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
            '^' if i == 0 => {
                patterns.push(Token::Bound(Meta::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(Token::Bound(Meta::End));
            }
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
            _ => {
                patterns.push(Token::Char(c));
            }
        }
    }
    patterns
}

type StatePtr = Rc<RefCell<State>>;

#[derive(Debug, Default)]
struct State {
    initial: bool,
    terminal: bool,
    transitions: HashMap<char, Vec<StatePtr>>,
}

const EPSILON: char = 'ε'; // Reserved for epsilon transitions
const ANY: char = '.'; // Reserved for "any character" transitions

// Parses an AST into an epsilon-NFA
fn ast_to_nfa(ast: &[Token]) -> (StatePtr, StatePtr) {
    let (start_state, mut end_state) = token_to_nfa(&ast[0]);
    for token in ast.iter().skip(1) {
        let (start_next, end_next) = token_to_nfa(token);
        add_transition(EPSILON, &end_state, &start_next);
        end_state = end_next;
    }
    let start = Rc::new(RefCell::new(State {
        initial: true,
        transitions: HashMap::from([(EPSILON, vec![Rc::clone(&start_state)])]),
        ..State::default()
    }));
    let end = Rc::new(RefCell::new(State {
        terminal: true,
        ..State::default()
    }));
    add_transition(EPSILON, &end_state, &end);
    (start, end)
}

fn token_to_nfa(token: &Token) -> (StatePtr, StatePtr) {
    let (start, end) = (
        Rc::new(RefCell::new(State::default())),
        Rc::new(RefCell::new(State::default())),
    );
    match token {
        Token::Char(c) => add_transition(*c, &start, &end),
        Token::Class(cls) => match cls {
            Meta::Digit => {
                for c in '0'..='9' {
                    add_transition(c, &start, &end);
                }
            }
            Meta::Alphanumeric => {
                for c in ('a'..='z')
                    .chain('A'..='Z')
                    .chain('0'..='9')
                    .chain(std::iter::once('_'))
                {
                    add_transition(c, &start, &end);
                }
            }
            Meta::Wildcard => {
                add_transition(ANY, &start, &end);
            }
            _ => unreachable!(),
        },
        Token::CharGroup { chars, negate } => {
            if *negate {
                add_transition(ANY, &start, &end);
                let deadend = Rc::new(RefCell::new(State::default()));
                for token in chars {
                    if let Token::Char(c) = token {
                        add_transition(*c, &start, &deadend);
                    } else {
                        panic!("[^...] group must only contain literal characters")
                    }
                }
            } else {
                for token in chars {
                    if let Token::Char(c) = token {
                        add_transition(*c, &start, &end);
                    } else {
                        panic!("[...] group must only contains literal characters")
                    }
                }
            }
        }
        Token::Bound(bound) => match bound {
            Meta::Start => add_transition(START_OF_TEXT, &start, &end),
            Meta::End => add_transition(END_OF_TEXT, &start, &end),
            _ => unreachable!(),
        },
        Token::CaptureGroup {
            alternates,
            index: _index,
        } => {
            for ast in alternates {
                let (alt_start, mut alt_end) = token_to_nfa(&ast[0]);
                for token in ast.iter().skip(1) {
                    let (start_next, end_next) = token_to_nfa(token);
                    add_transition(EPSILON, &alt_end, &start_next);
                    alt_end = end_next;
                }
                add_transition(EPSILON, &start, &alt_start);
                add_transition(EPSILON, &alt_end, &end);
            }
        }
        Token::Quantifier { token, kind } => match kind {
            Meta::ZeroOrOne => {
                add_transition(EPSILON, &start, &end);
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
            }
            Meta::OneOrMore => {
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
                add_transition(EPSILON, &token_end, &token_start);
            }
            Meta::ZeroOrMore => {
                add_transition(EPSILON, &start, &end);
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
                add_transition(EPSILON, &token_end, &token_start);
            }
            _ => unreachable!(),
        },
        Token::BackRef(_index) => unimplemented!(),
    }
    (start, end)
}

const START_OF_TEXT: char = '^';
const END_OF_TEXT: char = '$';

fn get_char(text: &[char], pos: usize) -> char {
    if pos == text.len() {
        return END_OF_TEXT;
    }
    text[pos]
}

// NOTE: simulation needs to handle . ^ $ and backrefs
fn check(state: StatePtr, text: &[char], pos: usize) -> bool {
    let state_ref = state.borrow();

    if state_ref.terminal {
        return true;
    }

    let ch = get_char(text, pos);
    // eprintln!("{}", ch);

    // character transitions (includes ^ and $)
    if let Some(next_states) = state_ref.transitions.get(&ch) {
        for next_state in next_states {
            if check(Rc::clone(next_state), text, pos + 1) {
                return true;
            }
        }
    // any character transitions (exludes ^ and $)
    } else if ch != '^' && ch != '$' {
        if let Some(next_states) = state_ref.transitions.get(&ANY) {
            for next_state in next_states {
                if check(Rc::clone(next_state), text, pos + 1) {
                    return true;
                }
            }
        }
    }

    // epsilon transitions (consumes no chars)
    if let Some(next_states) = state_ref.transitions.get(&EPSILON) {
        for next_state in next_states {
            if check(Rc::clone(next_state), text, pos) {
                return true;
            }
        }
    }

    false
}

fn add_transition(c: char, start: &Rc<RefCell<State>>, end: &Rc<RefCell<State>>) {
    start
        .borrow_mut()
        .transitions
        .entry(c)
        .or_default()
        .push(Rc::clone(end));
}

fn match_pattern(text: &str, regex: &str) -> bool {
    let mut text = text.chars().collect::<Vec<char>>();
    let ast = string_to_ast(regex, &mut 1);
    let (start, _) = ast_to_nfa(&ast);

    if let Some(Token::Bound(Meta::End)) = ast.last() {
        text.push(END_OF_TEXT);
    }
    if let Some(Token::Bound(Meta::Start)) = ast.first() {
        text.insert(0, START_OF_TEXT);
        return check(start, text.as_slice(), 0);
    }

    eprintln!("{:?} ({})\n", text, text.len());
    eprintln!("{:?} ({})\n", ast, ast.len());
    print_nfa(&start);

    for i in 0..text.len() {
        if check(Rc::clone(&start), text.as_slice(), i) {
            return true;
        }
    }

    false
}

// Helpers

fn print_nfa(state: &StatePtr) {
    let mut visited = HashSet::new();
    let mut label_map = HashMap::new();
    let mut next_label = 0;
    print_nfa_inner(state, &mut visited, &mut label_map, &mut next_label);
    eprintln!();
}
fn print_nfa_inner(
    state: &StatePtr,
    visited: &mut HashSet<usize>,
    label_map: &mut HashMap<usize, usize>,
    next_label: &mut usize,
) {
    let id = Rc::as_ptr(state) as usize;
    let label = *label_map.entry(id).or_insert_with(|| {
        let l = *next_label;
        *next_label += 1;
        l
    });

    if visited.contains(&id) {
        return;
    }
    visited.insert(id);

    let state_ref = state.borrow();
    println!(
        "({}){}{}{}",
        label,
        if state_ref.initial { " initial" } else { "" },
        if state_ref.terminal { " terminal" } else { "" },
        if !state_ref.initial && !state_ref.terminal && state_ref.transitions.is_empty() {
            " deadend"
        } else {
            ""
        }
    );
    let mut transitions: Vec<_> = state_ref.transitions.iter().collect();
    transitions.sort_by_key(|(ch, _)| {
        if **ch == EPSILON {
            "\u{0000}".to_string() // Ensures ε comes first
        } else {
            ch.to_string()
        }
    });

    for (ch, targets) in transitions {
        let ch_display = if *ch == EPSILON {
            "ε".to_string()
        } else {
            ch.to_string()
        };
        print!("    {} -> [", ch_display);
        for target in targets {
            let target_id = Rc::as_ptr(target) as usize;
            let target_label = *label_map.entry(target_id).or_insert_with(|| {
                let l = *next_label;
                *next_label += 1;
                l
            });
            print!(" {} ", target_label);
        }
        println!("]");
    }

    for targets in state_ref.transitions.values() {
        for target in targets {
            print_nfa_inner(target, visited, label_map, next_label);
        }
    }
}

fn find_files_recursive(root: &str) -> Vec<String> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.path().display().to_string())
        .collect()
}

fn print_match(input: &str, fp: Option<&str>, show_prefix: bool) {
    if show_prefix {
        print!("{}:", fp.unwrap());
    }
    println!("{}", input);
}

// Argument Parsing

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'E')]
    expression: String,

    #[arg(short='r', default_value_t=false, action=clap::ArgAction::SetTrue)]
    recursive: bool,

    paths: Vec<String>,
}

// Main

fn main() {
    let args = Args::parse();
    if args.recursive && (args.paths.len() != 1 || !path::Path::new(&args.paths[0]).is_dir()) {
        eprintln!("Error: -r requires exactyly one path to a directory.");
        process::exit(1);
    }

    let mut found = false;
    match !args.paths.is_empty() {
        // filepaths or directory
        true => {
            let filepaths = if args.recursive {
                find_files_recursive(&args.paths[0])
            } else {
                args.paths
            };
            let show_prefix = filepaths.len() > 1 || args.recursive;
            for fp in &filepaths {
                // eprintln!("Processing {:?}...", fp);
                fs::read_to_string(fp)
                    .expect("unable to open file")
                    .lines()
                    .filter(|input| match_pattern(input, &args.expression))
                    .for_each(|input| {
                        print_match(input, Some(fp), show_prefix);
                        found = true;
                    });
            }
        }
        // stdin
        false => {
            // simple stdin case
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            if match_pattern(&input, &args.expression) {
                print_match(&input, None, false);
                found = true;
            }
        }
    }

    if found {
        process::exit(0)
    } else {
        process::exit(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn matches(text: &str, regex: &str) -> bool {
        match_pattern(text, regex)
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
    fn test_digit_edge_cases() {
        assert!(matches("0", r"\d"));
        assert!(matches("9", r"\d"));
        assert!(!matches(".", r"\d"));
        assert!(!matches("-", r"\d"));
        assert!(!matches("+", r"\d"));
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
    fn test_alphanumeric_edge_cases() {
        assert!(matches("_", r"\w"));
        assert!(!matches(" ", r"\w"));
        assert!(!matches("\n", r"\w"));
        assert!(!matches("\t", r"\w"));
    }

    #[test]
    fn test_positive_character_groups() {
        assert!(matches("l", "[apple]"));
        assert!(!matches("banana", "[cdefgh]"));
        assert!(!matches("[]", "[pineapple]"));
    }

    #[test]
    fn test_character_group_edge_cases() {
        assert!(matches("a", "[a]"));
        assert!(!matches("b", "[a]"));
        assert!(matches("a", "[aaa]"));
    }

    #[test]
    fn test_negative_character_groups() {
        assert!(matches("apple", "[^xyz]"));
        assert!(matches("apple", "[^abc]"));
        assert!(!matches("banana", "[^anb]"));
        assert!(matches("orange", "[^opq]"));
    }

    #[test]
    fn test_negative_character_group_edge_cases() {
        assert!(!matches("a", "[^a]"));
        assert!(matches("b", "[^a]"));
        assert!(!matches("x", "[^xyz]"));
        assert!(matches("a", "[^xyz]"));
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
    fn test_mixed_character_classes() {
        assert!(matches("a1", r"\w\d"));
        assert!(matches("1a", r"\d\w"));
        assert!(!matches("ab", r"\w\d"));
        assert!(matches("a1", r"\w[123]"));
        assert!(!matches("a4", r"\w[123]"));
    }

    #[test]
    fn test_start_of_string_anchor() {
        assert!(matches("log", "^log"));
        assert!(!matches("slog", "^log"));
    }

    #[test]
    fn test_start_anchor_edge_cases() {
        assert!(matches("anything", "^"));
        assert!(matches("aaa", "^a+"));
        assert!(!matches("baaa", "^a+"));
    }

    #[test]
    fn test_end_of_string_anchor() {
        assert!(matches("cat", "cat$"));
        assert!(!matches("cats", "cat$"));
    }

    #[test]
    fn test_end_anchor_edge_cases() {
        assert!(matches("anything", "$"));
        assert!(matches("aaa", "a+$"));
        assert!(!matches("aaab", "a+$"));
    }

    #[test]
    fn test_both_anchors() {
        assert!(matches("hello", "^hello$"));
        assert!(!matches("hello world", "^hello$"));
        assert!(!matches("say hello", "^hello$"));
        assert!(!matches("a", "^$"));
    }

    #[test]
    fn test_one_or_more_times() {
        assert!(matches("cat", "ca+t"));
        assert!(matches("caaats", "ca+at"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
        assert!(matches("a", "a+"));
        assert!(matches("aa", "a+a"));
        assert!(matches("aa", "aa+"));
    }

    #[test]
    fn test_one_or_more_edge_cases() {
        assert!(matches("123", r"\d+"));
        assert!(matches("1", r"\d+"));
        assert!(!matches("", r"\d+"));
        assert!(matches("aaa", "[abc]+"));
        assert!(!matches("xyz", "[abc]+"));
    }

    #[test]
    fn test_zero_or_more_times() {
        assert!(matches("cat", "ca*t"));
        assert!(matches("caaats", "ca*t"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
        assert!(matches("a", "a*"));
        assert!(matches("aa", "a*a"));
        assert!(matches("aa", "aa*"));
    }

    #[test]
    fn test_zero_or_more_edge_cases() {
        assert!(matches("b", "a*"));
        assert!(matches("hello", "a*hello"));
        assert!(matches("abc", r"\d*"));
    }

    #[test]
    fn test_zero_or_one_times() {
        assert!(matches("cat", "ca?t"));
        assert!(matches("act", "ca?t"));
        assert!(!matches("dog", "ca?t"));
        assert!(!matches("cag", "ca?t"));
    }

    #[test]
    fn test_zero_or_one_edge_cases() {
        assert!(matches("a1", r"\w?\d"));
        assert!(matches("1", r"\w?\d"));
        assert!(!matches("ab", r"\w?\d"));
        assert!(matches("hello", "a?hello"));
        assert!(matches("ahello", "a?hello"));
        assert!(matches("aahello", "a?hello"));
    }

    #[test]
    fn test_wildcard() {
        assert!(matches("cat", "c.t"));
        assert!(!matches("car", "c.t"));
        assert!(matches("goøö0Ogol", "g.+gol"));
        assert!(!matches("gol", "g.+gol"));
    }

    #[test]
    fn test_wildcard_edge_cases() {
        assert!(matches("c\nt", "c.t"));
        assert!(matches("c\tt", "c.t"));
        assert!(matches("c t", "c.t"));
        assert!(matches("anything", ".*"));
        assert!(matches("a", ".+"));
    }

    #[test]
    fn test_wildcard_with_anchors() {
        assert!(!matches("^", "."));
        assert!(!matches("$", "."));
        assert!(matches("abc", "^.+$"));
        assert!(!matches("", "^.+$"));
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

    #[test]
    fn test_alternation_edge_cases() {
        assert!(matches("a", "(a|b)"));
        assert!(matches("b", "(a|b)"));
        assert!(!matches("c", "(a|b)"));
    }

    #[test]
    fn test_nested_groups() {
        assert!(matches("abc", "((a)bc)"));
        assert!(!matches("xbc", "((a)bc)"));
        assert!(matches("cat", "(c(a|o)t)"));
        assert!(matches("cot", "(c(a|o)t)"));
        assert!(!matches("cit", "(c(a|o)t)"));
    }

    #[test]
    fn test_quantified_groups() {
        assert!(matches("abab", "(ab)+"));
        assert!(matches("ab", "(ab)+"));
        assert!(!matches("a", "(ab)+"));
        assert!(matches("hello", "(hi)?hello"));
        assert!(matches("hihello", "(hi)?hello"));
    }

    #[test]
    fn test_complex_patterns() {
        assert!(matches("user@domain.com", r"\w+@\w+\.\w+"));
        assert!(!matches("user.domain.com", r"\w+@\w+\.\w+"));
        assert!(matches("123-456-7890", r"\d\d\d-\d\d\d-\d\d\d\d"));
        assert!(!matches("12-456-7890", r"\d\d\d-\d\d\d-\d\d\d\d"));
    }

    #[test]
    fn test_multiple_quantifiers() {
        assert!(matches("aabb", "a+b+"));
        assert!(matches("aaabbb", "a+b+"));
        assert!(matches("ab", "a+b+"));
        assert!(matches("abc", "a?b*c+"));
        assert!(matches("bcccc", "a?b*c+"));
        assert!(!matches("ab", "a?b*c+"));
    }

    #[test]
    fn test_edge_case_combinations() {
        assert!(matches("aaaa", "^a+$"));
        assert!(!matches("baaa", "^a+$"));
        assert!(matches("1", r"(\d|[abc])"));
        assert!(matches("a", r"(\d|[abc])"));
        assert!(!matches("x", r"(\d|[abc])"));
    }

    // #[test]
    // fn test_backreference_basic() {
    //     assert!(matches("cat and cat", "(cat) and \\1"));
    //     assert!(!matches("cat and dog", "(cat) and \\1"));
    // }
    //
    // #[test]
    // fn test_backreference_complex_word_digit() {
    //     assert!(matches(
    //         "grep 101 is doing grep 101 times",
    //         "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
    //     ));
    //     assert!(!matches(
    //         "$?! 101 is doing $?! 101 times",
    //         "(\\w\\w\\w \\d\\d\\d) is doing \\1 times"
    //     ));
    //     assert!(!matches(
    //         "grep yes is doing grep yes times",
    //         "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
    //     ));
    // }
    //
    // #[test]
    // fn test_backreference_char_group() {
    //     assert!(matches(
    //         "abcd is abcd, not efg",
    //         "([abcd]+) is \\1, not [^xyz]+"
    //     ));
    //     assert!(!matches(
    //         "efgh is efgh, not efg",
    //         "([abcd]+) is \\1, not [^xyz]+"
    //     ));
    //     assert!(!matches(
    //         "abcd is abcd, not xyz",
    //         "([abcd]+) is \\1, not [^xyz]+"
    //     ));
    // }
    //
    // #[test]
    // fn test_backreference_anchors() {
    //     assert!(matches(
    //         "this starts and ends with this",
    //         "^(\\w+) starts and ends with \\1$"
    //     ));
    //     assert!(!matches(
    //         "that starts and ends with this",
    //         "^(this) starts and ends with \\1$"
    //     ));
    //     assert!(!matches(
    //         "this starts and ends with this?",
    //         "^(this) starts and ends with \\1$"
    //     ));
    // }
    //
    // #[test]
    // fn test_backreference_quantifier() {
    //     assert!(matches(
    //         "once a dreaaamer, always a dreaaamer",
    //         "once a (drea+mer), alwaysz? a \\1"
    //     ));
    //     assert!(!matches(
    //         "once a dremer, always a dreaaamer",
    //         "once a (drea+mer), alwaysz? a \\1"
    //     ));
    //     assert!(!matches(
    //         "once a dreaaamer, alwayszzz a dreaaamer",
    //         "once a (drea+mer), alwaysz? a \\1"
    //     ));
    // }
    //
    // #[test]
    // fn test_backreference_alternation() {
    //     assert!(matches(
    //         "bugs here and bugs there",
    //         "(b..s|c..e) here and \\1 there"
    //     ));
    //     assert!(!matches(
    //         "bugz here and bugs there",
    //         "(b..s|c..e) here and \\1 there"
    //     ));
    // }
}
