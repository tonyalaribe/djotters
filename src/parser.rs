use std::collections::HashMap;

use crate::{Markdown, MarkdownAttributes};
use crate::MarkdownInline;
use crate::MarkdownText;

use nom::{sequence::separated_pair, bytes::complete::{take_until, take_till}, character::complete::{multispace0, anychar}, combinator::{eof, peek, verify, rest, recognize, value, success, cond, map_res, map_parser}, Parser, multi::many_till};
use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_not, tag, take, take_while1, take_while},
    character::{is_digit, complete::{multispace1, alphanumeric1}},
    combinator::{map, not, self, opt},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};


pub fn parse_markdown(i: &str) -> IResult<&str, Vec<Markdown>> {
    many1(alt((
        map(tag("\n"), |e| Markdown::LineBreak ),
        map(parse_header, |e| Markdown::Heading(e.0, e.1, e.2)),
        map(parse_unordered_list, |e| Markdown::UnorderedList(e.0, e.1)),
        map(parse_ordered_list, |e| Markdown::OrderedList(e.0, e.1)),
        map(parse_code_block, |e| {
            Markdown::Codeblock(e.0.0, e.1, e.0.1)
        }),
        map(parse_markdown_paragraph, |e| Markdown::Line(e.0,e.1)),
    )))(i)
}

fn match_surround_text<'a>(i: &'a str, surrounder: &'a str) -> IResult<&'a str, &'a str> {
    let not_surrounder = take_while1(move |c| c != '\n' && c != surrounder.chars().next().unwrap());

    let no_end_space = move |input: &'a str| {
        let (input, text) = not_surrounder(input)?;
        if text.ends_with(' ') 
        || text.starts_with(' ') 
        || text.ends_with('\u{a0}') // Unicode space
        || text.starts_with('\u{a0}') // Unicode space
        || text.contains("\n") // Should not contain a line break
        {
            Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char)))
        } else {
            Ok((input, text))
        }
    };

    delimited(
        tag(surrounder), // Match opening surrounder and ensure no space after it
        no_end_space, // Match text that is not the surrounder
        tag(surrounder), // Ensure no space before closing surrounder and match it
    )(i)
}

fn parse_id(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, id) = delimited(tag("#"), alphanumeric1, multispace0)(input)?;
    Ok((remaining, ("id", id)))
}

fn parse_class(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, class) = delimited(tag("."), alphanumeric1, multispace0)(input)?;
    Ok((remaining, ("class", class)))
}


fn parse_key_value_pair(input: &str) -> IResult<&str, (&str, &str)> {
    let parse_key = alphanumeric1;
    let parse_value = delimited(tag("\""), take_while(|c| c != '"'), tag("\""));
    separated_pair(parse_key, tag("="), parse_value)(input)
}

fn parse_attributes1(input: &str) -> IResult<&str, HashMap<String, String>> {
    let parse_attribute = alt((parse_id, parse_class, parse_key_value_pair));
    let parse_attributes = many0(delimited(multispace0, parse_attribute, multispace0));

    let mut attributes_parser = delimited(
        tag("{"),
        map(parse_attributes, |attrs| {
            attrs.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
        }),
        tag("}")
    );

    attributes_parser(input)
}

fn vec_to_hashmap_concat<'a>(vec: Vec<(&'a str, &str)>) -> HashMap<&'a str, String> {
    let mut hashmap = HashMap::new();

    for (key, value) in vec {
        hashmap.entry(key).and_modify(|e| *e = format!("{} {}", e, value)).or_insert(value.to_string());
    }

    hashmap
}

fn parse_attributes(input: &str) -> IResult<&str, HashMap<&str, String>> {
    let parse_attribute = alt((parse_id, parse_class, parse_key_value_pair));
    let parse_attributes = many0(delimited(multispace0, parse_attribute, multispace0));

    let mut attributes_parser = delimited(
        tag("{"),
        map(parse_attributes, |attrs| vec_to_hashmap_concat(attrs) ),
        tag("}")
    );
    attributes_parser(input)
}

fn match_surround_with_attrs<'a>(separator: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, (&'a str, MarkdownAttributes<'a>)> + 'a {
    move |input: &'a str| {
        let (remaining, text) = match_surround_text(input, separator)?;
        let (remaining, attrs) = opt(parse_attributes)(remaining)?;
        
        Ok((remaining, (text, attrs)))
    }
}

fn parse_link(i: &str) -> IResult<&str, (&str, &str, MarkdownAttributes)> {
    tuple((
        delimited(tag("["), take_until("]"), tag("]")),
        delimited(tag("("), take_until(")"), tag(")")),
        opt(parse_attributes),
        )
    )(i)
}

fn parse_image(i: &str) -> IResult<&str, (&str, &str, MarkdownAttributes)> {
    tuple((
        delimited(tag("!["), take_until("]"), tag("]")),
        delimited(tag("("), take_until(")"), tag(")")),
        opt(parse_attributes),
    ))(i)
}

/// Return the remaining input.
///
/// This parser is similar to [`nom::combinator::rest`], but returns `Err(Err::Error((_, ErrorKind::Verify)))` if the input is empty.
pub fn rest1(s: &str) -> IResult<&str, &str> {
    verify(rest, |x: &str| !x.is_empty())(s)
}

/// Returns the *shortest* input slice until it matches a parser.
///
/// Returns `Err(Err::Error((_, ErrorKind::Eof)))` if the input doesn't match the parser.
pub fn take_before0<'a, FOutput, F>(f: F) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str>
where
    F: Parser<&'a str, FOutput, nom::error::Error<&'a str>>,
{
    recognize(many_till(anychar, peek(f)))
}

fn parse_plaintext(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, (&str, MarkdownAttributes)> {
    move |i: &str|{
        let (input, output) = recognize(
            many_till(
                is_not("\n"),
                peek(
                    alt(
                        (
                            parse_markdown_not_plain(accept_linebreak), 
                            // Not really a linebreak, but this is a stopgap against adding attribute
                            // to the MarkdownInline enum, when we're only using this to peek
                            map(parse_attributes,|_|MarkdownInline::LineBreak), 
                            map(tag("\n"), |_|MarkdownInline::LineBreak),
                            map(eof, |_|MarkdownInline::LineBreak),
                        )
                    )
                ),
            )
        )(i)?;
        
        if output == ""{
            Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)))
        }else{
            Ok((input, (output, None)))
        }
    }
}

fn parse_markdown_not_plain(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownInline> {
    move |i: &str|{
    println!("parse_markdown_not_plain: {i:?}");
    alt((
        map(match_surround_with_attrs("*"), |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::Italic(s, attr)
        }),
        map(match_surround_with_attrs("_"), |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::Italic(s, attr)
        }),
        map(match_surround_with_attrs("`"), |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::InlineCode(s, attr)
        }),
        map(match_surround_with_attrs("**"), |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::Bold(s, attr)
        }),
        map(match_surround_with_attrs("__"), |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::Bold(s, attr)
        }),
        map(parse_image, |(tag, url, attr): (&str, &str, MarkdownAttributes)| {
            MarkdownInline::Image(tag, url,attr)
        }),
        map_res(terminated(tag("\n"), peek(is_not("\n"))), |_|{
                if accept_linebreak {
                    Ok(MarkdownInline::LineBreak)
                } else {
                    Err(nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Tag)))
                }
        }),
        map(parse_link, |(tag, url, attr): (&str, &str, MarkdownAttributes)| {
            MarkdownInline::Link(tag, url, attr)
        }),
    ))(i)
    }
}

fn parse_markdown_inline(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownInline> {
    move |i:&str| {
        alt((
            parse_markdown_not_plain(accept_linebreak),
            map(parse_plaintext(accept_linebreak), |(s, attr): (&str, MarkdownAttributes)| MarkdownInline::Plaintext(s, attr)),
        ))(i)
    }
}

fn parse_markdown_text(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownText> {
    move |i:&str| {
        many1(parse_markdown_inline(accept_linebreak))(i)
    }
}

// fn parse_markdown_paragraph(i: &str) -> IResult<&str, (MarkdownText, MarkdownAttributes)> {
//     pair(terminated(parse_markdown_text, alt((tag("\n\n"),eof ))), opt(parse_attributes))(i)
// }

fn parse_markdown_paragraph(i: &str) -> IResult<&str, (MarkdownText, MarkdownAttributes)> {
    terminated(
        pair(parse_markdown_text(true), opt(preceded(tag("\n"), parse_attributes))),
        alt((tag("\n\n"),eof)),
    )(i)
}

// this guy matches the literal character #
fn parse_header_tag(i: &str) -> IResult<&str, usize> {
    map(
        terminated(take_while1(|c| c == '#'), tag(" ")),
        |s: &str| s.len(),
    )(i)
}

// this combines a tuple of the header tag and the rest of the line
// # Heading title {.class #idName}\n
//
fn parse_header(i: &str) -> IResult<&str, (usize, MarkdownText, MarkdownAttributes)> {
    terminated(
        tuple(
            (parse_header_tag, parse_markdown_text(false), opt(parse_attributes))
        ),
        alt((tag("\n\n"),tag("\n"), eof))
    )(i)
}

fn parse_unordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(tag("-"), tag(" "))(i)
}

fn parse_unordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    preceded(parse_unordered_list_tag, parse_markdown_text(false))(i)
}

fn parse_unordered_list(i: &str) -> IResult<&str, (Vec<MarkdownText>, MarkdownAttributes)> {
    pair(many1(parse_unordered_list_element),opt(parse_attributes))(i)
}

fn parse_ordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(
        terminated(take_while1(|d| is_digit(d as u8)), tag(".")),
        tag(" "),
    )(i)
}

fn parse_ordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    preceded(parse_ordered_list_tag, parse_markdown_text(false))(i)
}

fn parse_ordered_list(i: &str) -> IResult<&str, (Vec<MarkdownText>, MarkdownAttributes)> {
    pair(many1(parse_ordered_list_element), opt(parse_attributes))(i)
}

fn parse_code_block(i: &str) -> IResult<&str, ((&str, MarkdownAttributes), &str)> {
    tuple((parse_code_block_lang, parse_code_block_body))(i)
}

fn parse_code_block_body(i: &str) -> IResult<&str, &str> {
    delimited(tag("\n"), is_not("```"), tag("```"))(i)
}

fn parse_code_block_lang(i: &str) -> IResult<&str, (&str, MarkdownAttributes)> {
    alt((
        preceded(tag("```"), parse_plaintext(true)),
        map(tag("```"), |_| ("__UNKNOWN__", None)),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{error::Error, error::ErrorKind, Err as NomErr};

    #[test]
    fn test_parse_attributes(){
        assert_eq!(parse_attributes("{ id=\"abc\" b=\"c\" }"),
            Ok(("", HashMap::from([("id".into(), "abc".into()), ("b".into(), "c".into())])))
        );

        assert_eq!(parse_attributes("{#idName .class1 .class2 b=\"c\" }"),
            Ok(("", HashMap::from([
                    ("id".into(), "idName".into()),
                    ("class".into(), "class1 class2".into()),
                    ("b".into(), "c".into())
                ])
            ))
        );

        assert_eq!(parse_attributes("{#idName .class1 .class2 id=\"abc\" b=\"c\" }"),
            Ok(("", HashMap::from([("b".into(), "c".into()), ("id".into(), "idName abc".into()), ("class".into(), "class1 class2".into())])))
        )
    }

    #[test]
    fn test_surround_with_attrs() {
        assert_eq!(
            match_surround_with_attrs("*")("*here is italic*"),
            Ok(("", ("here is italic", None)))
        );
        assert_eq!(
            match_surround_with_attrs("*")("*here is italic*{ id=\"abc\" b=\"c\" }"),
            Ok(("", ("here is italic", Some(HashMap::from([("id".into(), "abc".into()), ("b".into(), "c".into())])))))
        );
    }

    #[test]
    fn test_parse_markdown_inline(){
        let x = parse_plaintext(false)("here is *italic*");
        println!("parse plain {x:?}");

        assert_eq!(
            parse_markdown_inline(false)("here is *italic*"),
            Ok(("*italic*", MarkdownInline::Plaintext("here is ", None)))
        );
        assert_eq!(
            parse_markdown_inline(false)("*here is italic*"),
            Ok(("", MarkdownInline::Italic("here is italic", None)))
        );
        assert_eq!(
            parse_markdown_inline(false)("* here is italic*"),
            Ok(("", MarkdownInline::Plaintext("* here is italic*", None)))
        );
        assert_eq!(
            parse_markdown_inline(false)("* here is italic *"),
            Ok(("", MarkdownInline::Plaintext("* here is italic *", None)))
        );
        assert_eq!(
            parse_markdown_inline(false)("*here is italic*{ id=\"abc\" b=\"c\" }"),
            Ok(("", MarkdownInline::Italic("here is italic", Some(HashMap::from([("id".into(), "abc".into()), ("b".into(), "c".into())])))))
        );
        assert_eq!(
            parse_markdown_inline(false)("*here is \nitalic*"),
            Ok(("\nitalic*", MarkdownInline::Plaintext("*here is ", None)))
        )
    }

    #[test]
    fn test_parse_markdown_text(){
        assert_eq!(parse_markdown_not_plain(false)("*here is italic*"),
            Ok(("", MarkdownInline::Italic("here is italic", None)))
        );
        assert_eq!(parse_plaintext(false)("*here is \nitalic*"),
            Ok(("\nitalic*", ("*here is ",  None)))
        );
        assert_eq!(parse_plaintext(true)(""),
             Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic*"),
            Ok(("", vec![MarkdownInline::Italic("here is italic", None)]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic* lorem ipsum"),
            Ok(("", vec![MarkdownInline::Italic("here is italic", None), MarkdownInline::Plaintext(" lorem ipsum", None)]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here*italic"),
            Ok(("", vec![MarkdownInline::Italic("here", None), MarkdownInline::Plaintext("italic", None)]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic* lorem ipsum"),
            Ok(("", vec![MarkdownInline::Italic("here is italic", None), MarkdownInline::Plaintext(" lorem ipsum", None)]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic * lorem ipsum"),
            Ok(("", vec![MarkdownInline::Plaintext("*here is italic * lorem ipsum", None)]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic*{ id=\"abc\" b=\"c\" }"),
            Ok(("", vec![MarkdownInline::Italic("here is italic", Some(HashMap::from([("id".into(), "abc".into()), ("b".into(), "c".into())])))]))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic*{ .className }"),
            Ok(("", vec![MarkdownInline::Italic("here is italic", Some(HashMap::from([("class".into(), "className".into())])))]))
        );
    }

    #[test]
    fn test_parse_markdown_block(){
        assert_eq!(parse_markdown("*here*italics"),
            Ok(("", vec![Markdown::Line(vec![MarkdownInline::Italic("here", None), MarkdownInline::Plaintext("italics", None)], None)]))
        );
        // assert_eq!(parse_markdown("foo*bar*"),
        //     Ok(("", vec![Markdown::Line(vec![MarkdownInline::Plaintext("foo", None), MarkdownInline::Italic("bar", None)], None)]))
        // );
        assert_eq!(
            parse_header("# The header"), 
            Ok(("", (1, vec![MarkdownInline::Plaintext("The header", None)], None)))
        );
        assert_eq!(
            parse_header("# The \nheader"), 
            Ok(("header", (1, vec![MarkdownInline::Plaintext("The ", None)], None)))
        );
        assert_eq!(parse_markdown("## foo*bar*"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                ], None),
            ]))
        );
        assert_eq!(parse_markdown("## foo*bar* {.className}"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                    MarkdownInline::Plaintext(" ", None)
                ], Some(HashMap::from([("class".into(), "className".into())]))),
            ]))
        );
        assert_eq!(parse_markdown("## foo*bar*
Paragraph trial"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                ], None),
                Markdown::Line(vec![MarkdownInline::Plaintext("Paragraph trial", None)], None)
            ]))
        );
        assert_eq!(parse_markdown("## foo*bar*
Paragraph trial\n\nNew Paragraph"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                ], None),
                Markdown::Line(vec![MarkdownInline::Plaintext("Paragraph trial", None)], None),
                Markdown::Line(vec![MarkdownInline::Plaintext("New Paragraph", None)], None)
            ]))
        );
        assert_eq!(parse_markdown("## foo*bar* {.className}
Paragraph trial\n![](https://placehold.it/200x200)\n
"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                    MarkdownInline::Plaintext(" ", None),
                ], Some(HashMap::from([("class".into(), "className".into())]))),
                Markdown::Line(vec![
                    MarkdownInline::Plaintext("Paragraph trial", None),
                    MarkdownInline::LineBreak,
                    MarkdownInline::Image("", "https://placehold.it/200x200", None),
                ], None),
            ]))
        );
        assert_eq!(parse_markdown("## foo*bar*
Paragraph trial![]\n(https://placehold.it/200x200)
{.className}"),
            Ok(("", vec![
                Markdown::Heading(2, vec![
                    MarkdownInline::Plaintext("foo", None), 
                    MarkdownInline::Italic("bar", None),
                ], None),
                Markdown::Line(vec![
                    MarkdownInline::Plaintext("Paragraph trial", None),
                    MarkdownInline::LineBreak,
                    MarkdownInline::Image("", "https://placehold.it/200x200", None)
                ], None),
            ]))
        );

    }


    #[test]
    fn test_parse_markdown_document(){
        let document = "# Article name from Example site
## Current Category: BlaBla";
        let result = parse_markdown(document);
        println!("{result:?}")
    }

    #[test]
    fn test_parse_markdown_document2(){
        let document = "# Article name from Example site
## Current Category: BlaBla
<div>
    # subject
</div>

";
        let result = parse_markdown(document);
        println!("{result:?}")
    }

    #[test]
    fn test_parse_italics() {
        // assert_eq!(
        //     parse_markdown("*here is italic*"),
        //     Ok(("", ("here is italic", None)))
        // );

        // assert_eq!(
        //     parse_italics("*here is italic*{id=\"abc\"}"),
        //     Ok(
        //         ("", ("here is italic", Some(HashMap::from([("id", "abc")]))))
        //     )
        // );

        // assert_eq!(
        //     parse_italics("*here is italic*{a=\"b\" c=\"d\"}"),
        //     Ok(
        //         ("", ("here is italic", Some(HashMap::from([("a", "b"), ("c", "d")]))))
        //     )
        // );

        // assert_eq!(
        //     parse_italics("*here is italic"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );

        // assert_eq!(
        //     parse_italics("here is italic*"),
        //     Err(NomErr::Error(Error {
        //         input: "here is italic*",
        //         code: ErrorKind::Tag,
        //     }))
        // );
        // assert_eq!(
        //     parse_italics("here is italic"),
        //     Err(NomErr::Error(Error {
        //         input: "here is italic",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_italics("*"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::IsNot
        //     }))
        // );
        // assert_eq!(
        //     parse_italics("**"),
        //     Err(NomErr::Error(Error {
        //         input: "*",
        //         code: ErrorKind::IsNot
        //     }))
        // );
        // assert_eq!(
        //     parse_italics(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_italics("**we are doing bold**"),
        //     Err(NomErr::Error(Error {
        //         input: "*we are doing bold**",
        //         code: ErrorKind::IsNot
        //     }))
        // );
    }

//     #[test]
//     fn test_parse_boldtext() {
//         assert_eq!(parse_boldtext("**here is bold**"), Ok(("", "here is bold")));
//         assert_eq!(
//             parse_boldtext("**here is bold"),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("here is bold**"),
//             Err(NomErr::Error(Error {
//                 input: "here is bold**",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("here is bold"),
//             Err(NomErr::Error(Error {
//                 input: "here is bold",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("****"),
//             Err(NomErr::Error(Error {
//                 input: "**",
//                 code: ErrorKind::IsNot
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("**"),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::IsNot
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("*"),
//             Err(NomErr::Error(Error {
//                 input: "*",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_boldtext(""),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_boldtext("*this is italic*"),
//             Err(NomErr::Error(Error {
//                 input: "*this is italic*",
//                 code: ErrorKind::Tag
//             }))
//         );
//     }

//     #[test]
//     fn test_parse_inline_code() {
//         assert_eq!(
//             parse_boldtext("**here is bold**\n"),
//             Ok(("\n", "here is bold"))
//         );
//         assert_eq!(
//             parse_inline_code("`here is code"),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_inline_code("here is code`"),
//             Err(NomErr::Error(Error {
//                 input: "here is code`",
//                 code: ErrorKind::Tag
//             }))
//         );
//         assert_eq!(
//             parse_inline_code("``"),
//             Err(NomErr::Error(Error {
//                 input: "`",
//                 code: ErrorKind::IsNot
//             }))
//         );
//         assert_eq!(
//             parse_inline_code("`"),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::IsNot
//             }))
//         );
//         assert_eq!(
//             parse_inline_code(""),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//     }

//     #[test]
//     fn test_parse_link() {
//         assert_eq!(
//             parse_link("[title](https://www.example.com)"),
//             Ok(("", ("title", "https://www.example.com")))
//         );
//         assert_eq!(
//             parse_inline_code(""),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//     }

    #[test]
    fn test_parse_image() {
        assert_eq!(
            parse_image("![alt text](image.jpg)"),
            Ok(("", ("alt text", "image.jpg", None)))
        );
        assert_eq!(
            parse_image("![](image.jpg)"),
            Ok(("", ("", "image.jpg", None)))
        );
        // assert_eq!(
        //     parse_inline_code(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
    }

    #[test]
    fn test_parse_plaintext() {
        assert_eq!(
            parse_plaintext(true)("1234567890"),
            Ok(("", ("1234567890", None)))
        );
        assert_eq!(
            parse_plaintext(true)("oh my gosh!"),
            Ok(("", ("oh my gosh!", None)))
        );
        assert_eq!(
            parse_plaintext(true)("oh my gosh![](abc.jpg)"),
            Ok(("![](abc.jpg)", ("oh my gosh", None)))
        );
        assert_eq!(
            parse_plaintext(true)("oh my gosh!*"),
            Ok(("", ("oh my gosh!*", None)))
        );
        assert_eq!(
            parse_plaintext(true)("*bold babey bold*"),
            Err(NomErr::Error(Error {
                input: "*bold babey bold*",
                code: ErrorKind::Eof // should be ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("[link babey](and then somewhat)"),
            Err(NomErr::Error(Error {
                input: "[link babey](and then somewhat)",
                code: ErrorKind::Eof // ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("`codeblock for bums`"),
            Err(NomErr::Error(Error {
                input: "`codeblock for bums`",
                code: ErrorKind::Eof // ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("![ but wait theres more](jk)"),
            Err(NomErr::Error(Error {
                input: "![ but wait theres more](jk)",
                code: ErrorKind::Eof // ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("here is plaintext"),
            Ok(("", ("here is plaintext", None)))
        );
        assert_eq!(
            parse_plaintext(true)("here is plaintext!"),
            Ok(("", ("here is plaintext!", None)))
        );
        assert_eq!(
            parse_plaintext(true)("here is plaintext![image starting"),
            Ok(("", ("here is plaintext![image starting", None)))
        );
        assert_eq!(
            parse_plaintext(true)("here is plaintext\n"),
            Ok(("\n", ("here is plaintext", None)))
        );
        assert_eq!(
            parse_plaintext(true)("*here is italic*"),
            Err(NomErr::Error(Error {
                input: "*here is italic*",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_plaintext(true)("**here is bold**"),
            Err(NomErr::Error(Error {
                input: "**here is bold**",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_plaintext(true)("`here is code`"),
            Err(NomErr::Error(Error {
                input: "`here is code`",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_plaintext(true)("[title](https://www.example.com)"),
            Err(NomErr::Error(Error {
                input: "[title](https://www.example.com)",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_plaintext(true)("![alt text](image.jpg)"),
            Err(NomErr::Error(Error {
                input: "![alt text](image.jpg)",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_plaintext(true)(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Eof
            }))
        );
    }

//     #[test]
//     fn test_parse_markdown_inline() {
//         assert_eq!(
//             parse_markdown_inline("*here is italic*"),
//             Ok(("", MarkdownInline::Italic("here is italic", None)))
//         );
//         assert_eq!(
//             parse_markdown_inline("**here is bold**"),
//             Ok(("", MarkdownInline::Bold("here is bold", None)))
//         );
//         assert_eq!(
//             parse_markdown_inline("`here is code`"),
//             Ok(("", MarkdownInline::InlineCode("here is code", None)))
//         );
//         assert_eq!(
//             parse_markdown_inline("[title](https://www.example.com)"),
//             Ok((
//                 "",
//                 (MarkdownInline::Link(
//                     "title",
//                     "https://www.example.com",
//                     None
//                 ))
//             ))
//         );
//         assert_eq!(
//             parse_markdown_inline("![alt text](image.jpg)"),
//             Ok((
//                 "",
//                 (MarkdownInline::Image("alt text", "image.jpg", None))
//             ))
//         );
//         assert_eq!(
//             parse_markdown_inline("here is plaintext!"),
//             Ok((
//                 "",
//                 MarkdownInline::Plaintext("here is plaintext!", None)
//             ))
//         );
//         assert_eq!(
//             parse_markdown_inline("here is some plaintext *but what if we italicize?"),
//             Ok((
//                 "*but what if we italicize?",
//                 MarkdownInline::Plaintext("here is some plaintext ", None)
//             ))
//         );
//         assert_eq!(
//             parse_markdown_inline(
//                 r#"here is some plaintext 
// *but what if we italicize?"#
//             ),
//             Ok((
//                 "\n*but what if we italicize?",
//                 MarkdownInline::Plaintext("here is some plaintext ", None)
//             ))
//         );
//         // assert_eq!(
//         //     parse_markdown_inline("\n"),
//         //     Err(NomErr::Error(Error {
//         //         input: "\n",
//         //         code: ErrorKind::Not
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_markdown_inline(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Eof
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_markdown_text() {
//         assert_eq!(parse_markdown_text("\n"), Ok(("", vec![])));
//         assert_eq!(
//             parse_markdown_text("here is some plaintext\n"),
//             Ok((
//                 "",
//                 vec![MarkdownInline::Plaintext("here is some plaintext", None)]
//             ))
//         );
//         assert_eq!(
//             parse_markdown_text("here is some plaintext *but what if we italicize?*\n"),
//             Ok((
//                 "",
//                 vec![
//                     MarkdownInline::Plaintext("here is some plaintext ", None),
//                     MarkdownInline::Italic("but what if we italicize?", None),
//                 ]
//             ))
//         );
//         assert_eq!(
//             parse_markdown_text("here is some plaintext *but what if we italicize?* I guess it doesnt **matter** in my `code`\n"),
//             Ok(("", vec![
//                 MarkdownInline::Plaintext("here is some plaintext ", None),
//                 MarkdownInline::Italic("but what if we italicize?", None),
//                 MarkdownInline::Plaintext(" I guess it doesnt ", None),
//                 MarkdownInline::Bold("matter", None),
//                 MarkdownInline::Plaintext(" in my ", None),
//                 MarkdownInline::InlineCode("code", None),
//             ]))
//         );
//         assert_eq!(
//             parse_markdown_text("here is some plaintext *but what if we italicize?*\n"),
//             Ok((
//                 "",
//                 vec![
//                     MarkdownInline::Plaintext("here is some plaintext ", None),
//                     MarkdownInline::Italic("but what if we italicize?", None),
//                 ]
//             ))
//         );
//         // assert_eq!(
//         //     parse_markdown_text("here is some plaintext *but what if we italicize?"),
//         //     Err(NomErr::Error(Error {
//         //         input: "*but what if we italicize?",
//         //         code: ErrorKind::Tag
//         //     })) // Ok(("*but what if we italicize?", vec![MarkdownInline::Plaintext(String::from("here is some plaintext "))]))
//         // );
//     }

//     #[test]
//     fn test_parse_header_tag() {
//         assert_eq!(parse_header_tag("# "), Ok(("", 1)));
//         assert_eq!(parse_header_tag("### "), Ok(("", 3)));
//         assert_eq!(parse_header_tag("# h1"), Ok(("h1", 1)));
//         assert_eq!(parse_header_tag("# h1"), Ok(("h1", 1)));
//         assert_eq!(
//             parse_header_tag(" "),
//             Err(NomErr::Error(Error {
//                 input: " ",
//                 code: ErrorKind::TakeWhile1
//             }))
//         );
//         assert_eq!(
//             parse_header_tag("#"),
//             Err(NomErr::Error(Error {
//                 input: "",
//                 code: ErrorKind::Tag
//             }))
//         );
//     }

//     #[test]
//     fn test_parse_header() {
//         assert_eq!(
//             parse_header("# h1\n"),
//             Ok(("", (1, vec![MarkdownInline::Plaintext("h1", None)])))
//         );
//         assert_eq!(
//             parse_header("## h2\n"),
//             Ok(("", (2, vec![MarkdownInline::Plaintext("h2", None)])))
//         );
//         assert_eq!(
//             parse_header("###  h3\n"),
//             Ok((
//                 "",
//                 (3, vec![MarkdownInline::Plaintext(" h3", None)])
//             ))
//         );
//         // assert_eq!(
//         //     parse_header("###h3"),
//         //     Err(NomErr::Error(Error {
//         //         input: "h3",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_header("###"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_header(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::TakeWhile1
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_header("#"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(parse_header("# \n"), Ok(("", (1, vec![]))));
//         // assert_eq!(
//         //     parse_header("# test"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_unordered_list_tag() {
//         assert_eq!(parse_unordered_list_tag("- "), Ok(("", "-")));
//         assert_eq!(
//             parse_unordered_list_tag("- and some more"),
//             Ok(("and some more", "-"))
//         );
//         // assert_eq!(
//         //     parse_unordered_list_tag("-"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_unordered_list_tag("-and some more"),
//         //     Err(NomErr::Error(Error {
//         //         input: "and some more",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_unordered_list_tag("--"),
//         //     Err(NomErr::Error(Error {
//         //         input: "-",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_unordered_list_tag(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_unordered_list_element() {
//         assert_eq!(
//             parse_unordered_list_element("- this is an element\n"),
//             Ok((
//                 "",
//                 vec![MarkdownInline::Plaintext("this is an element", None)]
//             ))
//         );
//         assert_eq!(
//             parse_unordered_list_element(
//                 r#"- this is an element
// - this is another element
// "#
//             ),
//             Ok((
//                 "- this is another element\n",
//                 vec![MarkdownInline::Plaintext("this is an element", None)]
//             ))
//         );
//         // assert_eq!(
//         //     parse_unordered_list_element(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(parse_unordered_list_element("- \n"), Ok(("", vec![])));
//         // assert_eq!(
//         //     parse_unordered_list_element("- "),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_unordered_list_element("- test"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_unordered_list_element("-"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_unordered_list() {
//         // assert_eq!(
//         //     parse_unordered_list("- this is an element"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         assert_eq!(
//             parse_unordered_list("- this is an element\n"),
//             Ok((
//                 "",vec![vec![MarkdownInline::Plaintext("this is an element", None)]], None
//             ))
//         );
//         assert_eq!(
//             parse_unordered_list(
//                 r#"- this is an element
// - here is another
// "#
//             ),
//             Ok((
//                 "",
//                 vec![
//                     vec![MarkdownInline::Plaintext("this is an element", None)],
//                     vec![MarkdownInline::Plaintext("here is another", None)]
//                 ],
//                 None
//             ))
//         );
//     }

//     #[test]
//     fn test_parse_ordered_list_tag() {
//         assert_eq!(parse_ordered_list_tag("1. "), Ok(("", "1")));
//         assert_eq!(parse_ordered_list_tag("1234567. "), Ok(("", "1234567")));
//         assert_eq!(
//             parse_ordered_list_tag("3. and some more"),
//             Ok(("and some more", "3"))
//         );
//         // assert_eq!(
//         //     parse_ordered_list_tag("1"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_tag("1.and some more"),
//         //     Err(NomErr::Error(Error {
//         //         input: "and some more",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_tag("1111."),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_tag(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::TakeWhile1
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_ordered_list_element() {
//         assert_eq!(
//             parse_ordered_list_element("1. this is an element\n"),
//             Ok((
//                 "",
//                 vec![MarkdownInline::Plaintext("this is an element", None)]
//             ))
//         );
//         assert_eq!(
//             parse_ordered_list_element(
//                 r#"1. this is an element
// 1. here is another
// "#
//             ),
//             Ok((
//                 "1. here is another\n",
//                 vec![MarkdownInline::Plaintext("this is an element", None)]
//             ))
//         );
//         // assert_eq!(
//         //     parse_ordered_list_element(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::TakeWhile1
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_element(""),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::TakeWhile1
//         //     }))
//         // );
//         // assert_eq!(parse_ordered_list_element("1. \n"), Ok(("", vec![])));
//         // assert_eq!(
//         //     parse_ordered_list_element("1. test"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_element("1. "),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         // assert_eq!(
//         //     parse_ordered_list_element("1."),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//     }

//     #[test]
//     fn test_parse_ordered_list() {
//         assert_eq!(
//             parse_ordered_list("1. this is an element\n"),
//             Ok((
//                 "",
//                 vec![vec![MarkdownInline::Plaintext("this is an element", None)]]
//             ))
//         );
//         // assert_eq!(
//         //     parse_ordered_list("1. test"),
//         //     Err(NomErr::Error(Error {
//         //         input: "",
//         //         code: ErrorKind::Tag
//         //     }))
//         // );
//         assert_eq!(
//             parse_ordered_list(
//                 r#"1. this is an element
// 2. here is another
// "#
//             ),
//             Ok((
//                 "",
//                 vec![
//                     vec!(MarkdownInline::Plaintext("this is an element", None)),
//                     vec![MarkdownInline::Plaintext("here is another", None)]
//                 ]
//             ))
//         );
//     }

//     #[test]
//     fn test_parse_codeblock() {
//         assert_eq!(
//             parse_code_block(
//                 r#"```bash
// pip install foobar
// ```"#
//             ),
//             Ok((
//                 "",
//                 (
//                     "bash",
//                     r#"pip install foobar
// "#, 
//                     None
//                 )
//             ))
//         );
//         assert_eq!(
//             parse_code_block(
//                 r#"```python
// import foobar

// foobar.pluralize('word') # returns 'words'
// foobar.pluralize('goose') # returns 'geese'
// foobar.singularize('phenomena') # returns 'phenomenon'
// ```"#
//             ),
//             Ok((
//                 "",
//                 (
//                     String::from("python"),
//                     r#"import foobar

// foobar.pluralize('word') # returns 'words'
// foobar.pluralize('goose') # returns 'geese'
// foobar.singularize('phenomena') # returns 'phenomenon'
// "#
//                 )
//             ))
//         );
//         // assert_eq!(
//         // 	parse_code_block("```bash\n pip `install` foobar\n```"),
//         // 	Ok(("", "bash\n pip `install` foobar\n"))
//         // );
//     }

//     #[test]
//     fn test_parse_codeblock_no_language() {
//         assert_eq!(
//             parse_code_block(
//                 r#"```
// pip install foobar
// ```"#
//             ),
//             Ok((
//                 "",
//                 (
//                     String::from("__UNKNOWN__"),
//                     r#"pip install foobar
// "#
//                 )
//             ))
//         );
//     }

//     #[test]
//     fn test_parse_markdown() {
//         assert_eq!(
//             parse_markdown(
//                 r#"# Foobar

// Foobar is a Python library for dealing with word pluralization.

// ```bash
// pip install foobar
// ```
// ## Installation

// Use the package manager [pip](https://pip.pypa.io/en/stable/) to install foobar.
// ```python
// import foobar

// foobar.pluralize('word') # returns 'words'
// foobar.pluralize('goose') # returns 'geese'
// foobar.singularize('phenomena') # returns 'phenomenon'
// ```"#
//             ),
//             Ok((
//                 "",
//                 vec![
//                     Markdown::Heading(1, vec![MarkdownInline::Plaintext("Foobar", None)], None),
//                     Markdown::Line(vec![MarkdownInline::Plaintext(
//                         "Foobar is a Python library for dealing with word pluralization.",
//                         None
//                     )], None),
//                     Markdown::Codeblock("bash", "pip install foobar\n", None),
//                     Markdown::LineBreak,
//                     Markdown::Heading(
//                         2,
//                         vec![MarkdownInline::Plaintext("Installation", None)], 
//                         None
//                     ),
//                     Markdown::Line(vec![], None),
//                     Markdown::Line(vec![
//                         MarkdownInline::Plaintext("Use the package manager ", None),
//                         MarkdownInline::Link(
//                             "pip",
//                             "https://pip.pypa.io/en/stable/",
//                             None
//                         ),
//                         MarkdownInline::Plaintext(" to install foobar.", None),
//                     ], None),
//                     Markdown::Codeblock(
//                         "python",
//                         
//                             r#"import foobar

// foobar.pluralize('word') # returns 'words'
// foobar.pluralize('goose') # returns 'geese'
// foobar.singularize('phenomena') # returns 'phenomenon'
// "#, 
//                             None
//                         
//                     ),
//                 ]
//             ))
//         )
//     }
}
