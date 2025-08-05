use std::env;
use std::io;
use std::process;

fn match_char(input_line: &str, char: char) -> bool {
    return input_line.contains(char);
}

fn match_numeric(input_line: &str) -> bool {
    return input_line.chars().any(|c| c.is_ascii_digit());
}

fn match_alphanumeric(input_line: &str) -> bool {
    return input_line
        .chars()
        .any(|c| c.is_ascii_alphanumeric() || c == '_');
}

fn match_character_group(input_line: &str, char_group: &str, positive: bool) -> bool {
    return char_group
        .chars()
        .any(|char| !positive ^ match_pattern(input_line, &char.to_string()));
}

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let pattern_len = pattern.chars().count();
    eprintln!("Pattern: {} ({})", pattern, pattern_len);

    if pattern_len == 1 {
        return match_char(input_line, pattern.chars().next().unwrap());
    }
    if pattern == "\\d" {
        return match_numeric(input_line);
    }
    if pattern == "\\w" {
        return match_alphanumeric(input_line);
    }
    if pattern_len >= 3 && pattern.starts_with('[') && pattern.ends_with(']') {
        let (start, positive) = match pattern.chars().nth(1).unwrap() {
            '^' => (2, false),
            _ => (1, true),
        };
        let char_group = &pattern[start..pattern.len() - 1];
        eprintln!("Character Group: {} ({})", char_group, positive);
        if !positive && pattern_len == 3 {
            return true;
        }
        return match_character_group(input_line, char_group, positive);
    }

    panic!("Unhandled pattern: {}", pattern)
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

    // Uncomment this block to pass the first stage
    if match_pattern(&input_line, &pattern) {
        eprintln!("Match");
        process::exit(0)
    } else {
        eprintln!("No match");
        process::exit(1)
    }
}
