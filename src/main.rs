mod ast;
mod nfa;

use clap::Parser;
use std::fs;
use std::io;
use std::path;
use std::process;
use std::rc::Rc;
use walkdir::WalkDir;

fn match_pattern(text: &str, regex: &str) -> bool {
    let mut text_vec: Vec<char> = text.chars().collect();
    let ast = ast::string_to_ast(regex, &mut 1);
    let (nfa, _) = nfa::ast_to_nfa(&ast);

    // Modify input text to include start and end markers
    if let Some(ast::Token::Bound(ast::Meta::End)) = ast.last() {
        text_vec.push(nfa::END_OF_TEXT);
    }
    if let Some(ast::Token::Bound(ast::Meta::Start)) = ast.first() {
        text_vec.insert(0, nfa::START_OF_TEXT);
        // Simulate the ε-NFA only from the start of the text
        return nfa::simulate_nfa(nfa, &text_vec, 0, &mut nfa::MatchContext::default());
    }

    // Simulate the ε-NFA for each position in the text
    for i in 0..text_vec.len() {
        let mut context = nfa::MatchContext::default();
        if nfa::simulate_nfa(Rc::clone(&nfa), &text_vec, i, &mut context) {
            return true;
        }
    }

    false
}

// Helpers

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
        // filepaths or recursive directory search
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

    #[test]
    fn test_backreference_basic() {
        assert!(matches("cat and cat", "(cat) and \\1"));
        assert!(!matches("cat and dog", "(cat) and \\1"));
    }

    #[test]
    fn test_backreference_complex_word_digit() {
        assert!(matches(
            "grep 101 is doing grep 101 times",
            "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times",
            "(\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times",
            "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
    }

    #[test]
    fn test_backreference_char_group() {
        assert!(matches(
            "abcd is abcd, not efg",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
        assert!(!matches(
            "efgh is efgh, not efg",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
        assert!(!matches(
            "abcd is abcd, not xyz",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
    }

    #[test]
    fn test_backreference_anchors() {
        assert!(matches(
            "this starts and ends with this",
            "^(\\w+) starts and ends with \\1$"
        ));
        assert!(!matches(
            "that starts and ends with this",
            "^(this) starts and ends with \\1$"
        ));
        assert!(!matches(
            "this starts and ends with this?",
            "^(this) starts and ends with \\1$"
        ));
    }

    #[test]
    fn test_backreference_quantifier() {
        assert!(matches(
            "once a dreaaamer, always a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
        assert!(!matches(
            "once a dremer, always a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
        assert!(!matches(
            "once a dreaaamer, alwayszzz a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
    }

    #[test]
    fn test_backreference_alternation() {
        assert!(matches(
            "bugs here and bugs there",
            "(b..s|c..e) here and \\1 there"
        ));
        assert!(!matches(
            "bugz here and bugs there",
            "(b..s|c..e) here and \\1 there"
        ));
    }

    #[test]
    fn test_nested_backreferences() {
        // Test basic nested backreferences
        assert!(matches("abaab", "((a)b)\\2\\1")); // ab + a + ab = abaab
        assert!(!matches("abab", "((a)b)\\2\\1")); // ab + a + ab ≠ abab
        assert!(!matches("abba", "((a)b)\\2\\1")); // ab + a + ab ≠ abba

        // Test multiple levels of nesting
        assert!(matches("abccbcabc", "(a(b(c)))\\3\\2\\1")); // abc + c + bc + abc
        assert!(!matches("abcabc", "(a(b(c)))\\3\\2\\1")); // not enough chars
    }

    #[test]
    fn test_complex_nested_backreferences() {
        // Test nested groups with alternation
        assert!(matches("abxbabx", "(a(b|c)x)\\2\\1")); // abx + b + abx
        assert!(matches("acxcacx", "(a(b|c)x)\\2\\1")); // acx + c + acx
        assert!(!matches("abxcabx", "(a(b|c)x)\\2\\1")); // mixed b/c
        assert!(!matches("abxaby", "(a(b|c)x)\\2\\1")); // not enough chars
    }

    #[test]
    fn test_multiple_backreferences() {
        // Test multiple backreferences in one pattern
        assert!(matches(
            "3 red squares and 3 red circles",
            "(\\d+) (\\w+) squares and \\1 \\2 circles"
        ));
        assert!(!matches(
            "3 red squares and 4 red circles",
            "(\\d+) (\\w+) squares and \\1 \\2 circles"
        ));
        assert!(matches(
            "grep 101 is doing grep 101 times",
            "(\\w\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times",
            "(\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times",
            "(\\w\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_character_groups() {
        // Test multiple backreferences with character groups
        assert!(matches(
            "abc-def is abc-def, not efg",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
        assert!(!matches(
            "efg-hij is efg-hij, not efg",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
        assert!(!matches(
            "abc-def is abc-def, not xyz",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_anchors() {
        // Test multiple backreferences with anchors
        assert!(matches(
            "apple pie, apple and pie",
            "^(\\w+) (\\w+), \\1 and \\2$"
        ));
        assert!(!matches(
            "pineapple pie, pineapple and pie",
            "^(apple) (\\w+), \\1 and \\2$"
        ));
        assert!(!matches(
            "apple pie, apple and pies",
            "^(\\w+) (pie), \\1 and \\2$"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_quantifiers() {
        // Test multiple backreferences with quantifiers
        assert!(matches(
            "howwdy hey there, howwdy hey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
        assert!(!matches(
            "hody hey there, howwdy hey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
        assert!(!matches(
            "howwdy heeey there, howwdy heeey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_alternation() {
        // Test multiple backreferences with alternation
        assert!(matches(
            "cat and fish, cat with fish",
            "(c.t|d.g) and (f..h|b..d), \\1 with \\2"
        ));
        assert!(!matches(
            "bat and fish, cat with fish",
            "(c.t|d.g) and (f..h|b..d), \\1 with \\2"
        ));
    }

    #[test]
    fn test_advanced_nested_backreferences() {
        // Test complex nested backreferences from the actual test cases
        assert!(matches(
            "'cat and cat' is the same as 'cat and cat'",
            "('(cat) and \\2') is the same as \\1"
        ));
        assert!(!matches(
            "'cat and cat' is the same as 'cat and dog'",
            "('(cat) and \\2') is the same as \\1"
        ));

        assert!(matches(
            "grep 101 is doing grep 101 times, and again grep 101 times",
            "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times, and again $?! 101 times",
            "((\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times, and again grep yes times",
            "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
    }

    #[test]
    fn test_deeply_nested_backreferences() {
        // Test deeply nested patterns with multiple groups and character classes
        assert!(matches(
            "abc-def is abc-def, not efg, abc, or def",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
        assert!(!matches(
            "efg-hij is efg-hij, not klm, efg, or hij",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
        assert!(!matches(
            "abc-def is abc-def, not xyz, abc, or def",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
    }

    #[test]
    fn test_complex_nested_with_anchors() {
        // Test complex nested backreferences with anchors
        assert!(matches(
            "apple pie is made of apple and pie. love apple pie",
            "^((\\w+) (\\w+)) is made of \\2 and \\3. love \\1$"
        ));
        assert!(!matches(
            "pineapple pie is made of apple and pie. love apple pie",
            "^((apple) (\\w+)) is made of \\2 and \\3. love \\1$"
        ));
        assert!(!matches(
            "apple pie is made of apple and pie. love apple pies",
            "^((\\w+) (pie)) is made of \\2 and \\3. love \\1$"
        ));
    }

    #[test]
    fn test_complex_nested_with_quantifiers() {
        // Test complex nested backreferences with quantifiers
        assert!(matches(
            "'howwdy hey there' is made up of 'howwdy' and 'hey'. howwdy hey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
        assert!(!matches(
            "'hody hey there' is made up of 'hody' and 'hey'. hody hey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
        assert!(!matches(
            "'howwdy heeey there' is made up of 'howwdy' and 'heeey'. howwdy heeey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
    }

    #[test]
    fn test_complex_nested_with_alternation() {
        // Test complex nested backreferences with alternation
        assert!(matches(
            "cat and fish, cat with fish, cat and fish",
            "((c.t|d.g) and (f..h|b..d)), \\2 with \\3, \\1"
        ));
        assert!(!matches(
            "bat and fish, bat with fish, bat and fish",
            "((c.t|d.g) and (f..h|b..d)), \\2 with \\3, \\1"
        ));
    }
}
