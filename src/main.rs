extern crate num_rational;
#[macro_use]
extern crate nom;

use num_rational::Rational32;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::character::complete::not_line_ending;
use nom::combinator::opt;
use nom::combinator::map;
use nom::combinator::value;
use nom::sequence::preceded;
use nom::sequence::delimited;
use nom::IResult;

#[derive(Debug)]
enum PestoCommand<'a> {
    Ingredient {
        amount: Rational32,
        unit: Option<&'a str>,
        ingredient: &'a str,
    },
    Annotation(&'a str),
    Action(&'a str),
    Tool(&'a str),
    Result(&'a str),
    Alternative(&'a str),
    Unknown(&'a str),
}

fn opt_slash_num(s: &str) -> IResult<&str, Option<i32>> {
    let (s, n_s_opt) = opt(preceded(tag("/"), digit1))(s)?;
    if let Some(n_s) = n_s_opt {
        let n: i32 = n_s.parse().unwrap();
        Ok((s, Some(n)))
    } else {
        Ok((s, None))
    }
}

fn parse_mixed_numeral(s: &str) -> IResult<&str, Rational32> {
    let (s, a_s) = digit1(s)?;
    let a: i32 = a_s.parse().unwrap();
    let (s, b_opt) = opt_slash_num(s)?;
    if let Some(b) = b_opt {
        let (s, c_opt) = opt_slash_num(s)?;
        if let Some(c) = c_opt {
            Ok((
                s,
                Rational32::new(a.checked_mul(c).and_then(|v| v.checked_add(b)).unwrap(), c),
            ))
        } else {
            Ok((s, Rational32::new(a, b)))
        }
    } else {
        Ok((s, Rational32::new(a, 1)))
    }
}


fn take_until0_str(delim: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |s| {
        let index = s.find(delim).unwrap_or_else(|| s.len());
        let (chars, rest) = s.split_at(index);
        Ok((rest, chars))
    }
}

fn parse_opt_unit(s: &str) -> IResult<&str, Option<&str>> {
    alt((value(None, tag("_")), map(take_until0_str(" "), |s| Some(s))))(s)
}

fn parse_ingredient(s: &str) -> IResult<&str, PestoCommand> {
    let (s, _) = tag("+")(s)?;
    let (s, amount) = parse_mixed_numeral(s)?;
    let (s, _) = multispace1(s)?;
    let (s, unit) = parse_opt_unit(s)?;
    let (s, _) = multispace1(s)?;
    let (s, ingredient) = not_line_ending(s)?;
    Ok((s, PestoCommand::Ingredient{amount, unit, ingredient}))
}

fn parse_action(s: &str) -> IResult<&str, PestoCommand> {
    let (s, cmd_str) = delimited(tag("["), take_until0_str("]"), tag("]"))(s)?;
    Ok((s, PestoCommand::Action(cmd_str)))
}

fn parse_annotation(s: &str) -> IResult<&str, PestoCommand> {
    let (s, annotation_str) = delimited(tag("("), take_until0_str(")"), tag(")"))(s)?;
    Ok((s, PestoCommand::Annotation(annotation_str)))
}

fn parser(s: &str) -> IResult<&str, Vec<PestoCommand>> {
    let (s, _) = tag("%pesto")(s)?;
    let mut result = Vec::new();
    let mut rest = s;
    while rest.len() > 0 {
        let (s, _) = multispace1(rest)?;
        let (s, cmd) = alt((parse_annotation, parse_action, parse_ingredient))(s)?;
        result.push(cmd);
        rest = s;
    }
    Ok((s, result))
}

fn quoted_str_content(s: &str) -> IResult<&str, String> {
    let mut escaped = false;
    let mut out = String::new();
    for (idx, c) in s.char_indices() {
        match c {
            '\\' => {
                if escaped {
                    out.push(c);
                    escaped = false;
                } else {
                    escaped = true;
                }
            }
            '"' => {
                if escaped {
                    out.push(c);
                    escaped = false;
                } else {
                    return Ok((&s[0..idx], out));
                }
            }
            _ => {
                if escaped {
                    tag("Invalid escape")(&s[idx-1..idx+1])?;
                    unreachable!()
                }
                out.push(c);
            }
        }
    }
    Ok(("", s.into()))
}

fn quoted_str(s: &str) -> IResult<&str, String> {
    delimited(tag("\""), quoted_str_content, tag("\""))(s)
}

fn word(s: &str) -> IResult<&str, &str> {
    let end_idx = s.char_indices().find(|(idx,c)| c.is_whitespace()).map(|(idx,c)| idx).unwrap_or(s.len());
    if end_idx > 0 {
        Ok((&s[end_idx..], &s[0..end_idx]))
    } else {
        tag("word would be empty")("")
    }
}

fn empty_word(s: &str) -> IResult<&str, &str> {
    let mut chars_it = s.chars();
    let success: bool;
    if let Some('_') = chars_it.next() {
        if let Some(c) = chars_it.next() {
            success = c.is_whitespace()
        } else {
            success = true
        }
    } else {
        success = false
    }
    if success {
        Ok((&s[1..], ""))
    } else {
        tag("not empty word")("")
    }
}

fn main() {
    let mut args = std::env::args();
    args.next(); // ignore executable name
    let fname = args.next().expect("first arg must be the recipe to load");
    let recipe = std::fs::read_to_string(fname).unwrap();
    let x = parser(&recipe);
    println!("{:?}", x.unwrap().1);
}
