extern crate num_rational;
extern crate nom;

use num_rational::Rational32;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::character::complete::multispace1;
use nom::character::complete::not_line_ending;
use nom::combinator::opt;
use nom::combinator::map;
use nom::sequence::preceded;
use nom::IResult;

#[derive(Debug)]
enum PestoCommand<'a> {
    AddIngredient {
        amount: Rational32,
        unit: Option<&'a str>,
        ingredient: &'a str,
    },
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

fn chars_until_space(s: &str) -> IResult<&str, &str> {
    let index = s.find(' ').unwrap_or_else(|| s.len()); // TODO so far no tabs etc
    let (chars, rest) = s.split_at(index);
    Ok((rest, chars))
}

fn parse_opt_unit(s: &str) -> IResult<&str, Option<&str>> {
    alt((map(tag("_"), |_| None), map(chars_until_space, |s| Some(s))))(s)
}

fn parse_add_ingredient(s: &str) -> IResult<&str, PestoCommand> {
    let (s, _) = tag("+")(s)?;
    let (s, amount) = parse_mixed_numeral(s)?;
    let (s, _) = multispace1(s)?;
    let (s, unit) = parse_opt_unit(s)?;
    let (s, _) = multispace1(s)?;
    let (s, ingredient) = not_line_ending(s)?;
    Ok((s, PestoCommand::AddIngredient{amount, unit, ingredient}))
}

fn parser(s: &str) -> IResult<&str, &str> {
    let (s, _) = tag("%pesto")(s)?;
    let (s, _) = multispace1(s)?;
    unimplemented!()
}

fn main() {
    let x = parse_add_ingredient("+100 g grobes Weizenmehl");
    println!("Hello {:?}", x);
}
