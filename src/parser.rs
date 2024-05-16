extern crate slugify;
use crate::MarkdownInline;
use crate::MarkdownText;
use crate::{translator, Alignment};
use crate::{Markdown, MarkdownAttributes};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_until, take_while, take_while1},
    character::{
        complete::{
            alphanumeric1, anychar, char, line_ending, multispace0, multispace1, newline, space0,
        },
        is_digit,
    },
    combinator::{cut, eof, map, map_res, opt, peek, recognize, rest, verify},
    error::ParseError,
    multi::{many0, many1, many_till, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition, Parser,
};
use slugify::slugify;
use std::collections::HashMap;

pub fn parse_markdown<'a>(i: &'a str) -> IResult<&'a str, Vec<Markdown<'a>>> {
    many1(alt((
        // Should be replaced. Hard linebreak is \<newline>
        map(tag("\n\n"), |_e| Markdown::LineBreak),
        map(parse_header, |mut e| {
            Markdown::Heading(e.1, e.2.clone(), set_or_check_header_id(&mut e.0, e.2))
        }),
        map(parse_table, |e| Markdown::Table(e.0, e.1, e.2)),
        map(parse_unordered_list, |e| Markdown::UnorderedList(e.1, e.0)),
        map(parse_ordered_list, |e| Markdown::OrderedList(e.1, e.0)),
        map(parse_code_block, |e| {
            Markdown::Codeblock(e.0 .0, e.1, e.0 .1)
        }),
        map(parse_div, |e| Markdown::Div(e.0, e.1, e.2)),
        map(parse_markdown_paragraph, |e| Markdown::Line(e.1, e.0)),
    )))(i)
}

pub fn set_or_check_header_id<'a>(
    attrs: &mut Option<HashMap<&'a str, String>>,
    content: Vec<MarkdownInline<'a>>,
) -> MarkdownAttributes<'a> {
    // Check if attrs is Some and if it contains the key "id"
    if let Some(ref mut attrs_map) = attrs {
        if !attrs_map.contains_key("id") {
            // If "id" is not present, insert a new "id" with the slugified content
            let slug = slugify_md(content); // Assuming slugify_md is defined elsewhere
            attrs_map.insert("id", slug);
        }
    } else {
        // If attrs is None, create a new HashMap and insert the "id"
        let mut new_attrs = HashMap::new();
        let slug = slugify_md(content); // Assuming slugify_md is defined elsewhere
        new_attrs.insert("id", slug);
        *attrs = Some(new_attrs);
    }

    attrs.to_owned()
}

pub fn slugify_md(content: Vec<MarkdownInline>) -> String {
    slugify!(&translator::translate_text_raw(content))
}

pub fn block_ending(i: &str) -> IResult<&str, Vec<&str>, nom::error::Error<&str>> {
    alt((many1(line_ending), map(eof, |_| Vec::new())))(i)
}

fn parse_div(i: &str) -> IResult<&str, (Option<&str>, Vec<Markdown>, MarkdownAttributes)> {
    let (input, attr) = opt(terminated(parse_attributes, tag("\n")))(i)?;
    let (input, colon_count) = preceded(
        multispace0,
        map(take_while1(|c| c == ':'), |s: &str| s.len()),
    )(input)?;

    let closing_tag = ":".repeat(colon_count);
    let (input, label) = terminated(opt(preceded(tag(" "), take_until("\n"))), tag("\n"))(input)?;

    let (input, content) =
        take_before0(tuple((tag("\n"), tag(&*closing_tag), block_ending)))(input)?;

    let (input, _) = tuple((tag("\n"), multispace0, tag(&*closing_tag), block_ending))(input)?;
    let (_, content_md) = parse_markdown(content)?;
    Ok((input, (label, content_md, attr)))
}

fn match_surround_text<'a>(
    i: &'a str,
    opener: &'a str,
    closer: &'a str,
) -> IResult<&'a str, &'a str> {
    let not_surrounder = take_while1(move |c| c != '\n' && c != closer.chars().next().unwrap());
    let no_end_space = move |input: &'a str| {
        let (input, text) = not_surrounder(input)?;
        if text.ends_with(' ')
        || text.starts_with(' ')
        || text.ends_with('\u{a0}') // Unicode space
        || text.starts_with('\u{a0}') // Unicode space
        || text.contains("\n")
        {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Char,
            )))
        } else {
            Ok((input, text))
        }
    };

    delimited(
        tag(opener),  // Match opening surrounder and ensure no space after it
        no_end_space, // Match text that is not the surrounder
        tag(closer),  // Ensure no space before closing surrounder and match it
    )(i)
}

fn parse_id(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, id) = delimited(
        tag("#"),
        take_before0(alt((multispace1, tag("}")))),
        multispace0,
    )(input)?;
    Ok((remaining, ("id", id)))
}

fn parse_class(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, class) = delimited(
        tag("."),
        take_before0(alt((multispace1, tag("}")))),
        multispace0,
    )(input)?;
    Ok((remaining, ("class", class)))
}

fn parse_key_value_pair(input: &str) -> IResult<&str, (&str, &str)> {
    let parse_key = alphanumeric1;
    let parse_value = delimited(tag("\""), take_while(|c| c != '"'), tag("\""));
    separated_pair(parse_key, tag("="), parse_value)(input)
}

fn vec_to_hashmap_concat<'a>(vec: Vec<(&'a str, &str)>) -> HashMap<&'a str, String> {
    let mut hashmap = HashMap::new();

    for (key, value) in vec {
        hashmap
            .entry(key)
            .and_modify(|e| *e = format!("{} {}", e, value))
            .or_insert(value.to_string());
    }

    hashmap
}

fn parse_attributes(input: &str) -> IResult<&str, HashMap<&str, String>> {
    let parse_attribute = alt((parse_id, parse_class, parse_key_value_pair));
    let parse_attributes = many0(delimited(multispace0, parse_attribute, multispace0));

    let mut attributes_parser = delimited(
        tag("{"),
        map(parse_attributes, |attrs| vec_to_hashmap_concat(attrs)),
        tag("}"),
    );
    attributes_parser(input)
}

fn match_surround2_with_attrs<'a>(
    recurse: bool,
    opener: &'a str,
    closer: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, (MarkdownText<'a>, MarkdownAttributes<'a>)> + 'a {
    move |input: &'a str| {
        let (remaining, text) = match_surround_text(input, opener, closer)?;
        let (remaining, attrs) = opt(parse_attributes)(remaining)?;
        let (_, child_elements) = if recurse {
            parse_markdown_text(false)(text)?
        } else {
            ("", vec![MarkdownInline::Plaintext(text, None)])
        };
        Ok((remaining, (child_elements, attrs)))
    }
}

fn parse_link(i: &str) -> IResult<&str, (&str, &str, MarkdownAttributes)> {
    tuple((
        delimited(tag("["), take_until("]"), tag("]")),
        delimited(tag("("), take_until(")"), tag(")")),
        opt(parse_attributes),
    ))(i)
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

fn parse_plaintext(
    accept_linebreak: bool,
) -> impl Fn(&str) -> IResult<&str, (&str, MarkdownAttributes)> {
    move |i: &str| {
        if i.len() == 0 {
            Err(nom::Err::Error(nom::error::Error::new(
                i,
                nom::error::ErrorKind::Eof,
            )))
        } else {
            let (input, output) = take_before0(alt((
                parse_markdown_not_plain(accept_linebreak),
                // Not really a linebreak, but this is a stopgap against adding attribute
                // to the MarkdownInline enum, when we're only using this to peek
                map(parse_attributes, |_| MarkdownInline::LineBreak),
                map(tag("\n"), |_| MarkdownInline::LineBreak),
                map(eof, |_| MarkdownInline::LineBreak),
            )))(i)?;
            if output.len() == 0 {
                Err(nom::Err::Error(nom::error::Error::new(
                    i,
                    nom::error::ErrorKind::Not,
                )))
            } else {
                Ok((input, (output, None)))
            }
        }
    }
}

fn parse_markdown_not_plain(
    accept_linebreak: bool,
) -> impl Fn(&str) -> IResult<&str, MarkdownInline> {
    move |i: &str| {
        alt((
            map(
                match_surround2_with_attrs(true, "{*", "*}"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Bold(s, attr),
            ),
            map(
                match_surround2_with_attrs(true, "{_", "_}"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Italic(s, attr),
            ),
            map(
                match_surround2_with_attrs(true, "*", "*"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Bold(s, attr),
            ),
            map(
                match_surround2_with_attrs(true, "_", "_"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Italic(s, attr),
            ),
            map(
                match_surround2_with_attrs(false, "`", "`"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::InlineCode(s, attr),
            ),
            // TODO: remove. not needed for djot
            map(
                match_surround2_with_attrs(true, "**", "**"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Bold(s, attr),
            ),
            // TODO: remove. not needed for djot
            map(
                match_surround2_with_attrs(true, "__", "__"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Bold(s, attr),
            ),
            map(
                parse_image,
                |(tag, url, attr): (&str, &str, MarkdownAttributes)| {
                    MarkdownInline::Image(tag, url, attr)
                },
            ),
            map(tag("\\\n"), |_| MarkdownInline::LineBreak),
            map_res(terminated(tag("\n"), peek(is_not("\n"))), |_| {
                if accept_linebreak {
                    Ok(MarkdownInline::LineBreak)
                } else {
                    Err(nom::Err::Error(nom::error::Error::new(
                        i,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            }),
            map(
                parse_link,
                |(tag, url, attr): (&str, &str, MarkdownAttributes)| {
                    MarkdownInline::Link(tag, url, attr)
                },
            ),
            map(
                match_surround2_with_attrs(true, "[", "]"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Span(s, attr),
            ),
        ))(i)
    }
}

fn parse_markdown_inline(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownInline> {
    move |i: &str| {
        alt((
            parse_markdown_not_plain(accept_linebreak),
            map(
                parse_plaintext(accept_linebreak),
                |(s, attr): (&str, MarkdownAttributes)| MarkdownInline::Plaintext(s, attr),
            ),
        ))(i)
    }
}

fn parse_markdown_text(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownText> {
    move |i: &str| many1(parse_markdown_inline(accept_linebreak))(i)
}

// A paragraph can include optional attributes, which will be on the line before it
// ```
// {.className #idName attr="attrVal" attr2=attrVal2}
// The paragraph content which can include *many* _inline_ {=items=}.
// ```
fn parse_markdown_paragraph(i: &str) -> IResult<&str, (MarkdownAttributes, MarkdownText)> {
    terminated(
        pair(
            opt(terminated(parse_attributes, tag("\n"))),
            parse_markdown_text(true),
        ),
        block_ending,
    )(i.trim_start())
}

// this guy matches the literal character #
fn parse_header_tag(i: &str) -> IResult<&str, usize> {
    map(
        terminated(take_while1(|c| c == '#'), tag(" ")),
        |s: &str| s.len(),
    )(i)
}

// this combines a tuple of the header tag and the rest of the line
// {.class #idName}
// # Heading title\n
//
fn parse_header(i: &str) -> IResult<&str, (MarkdownAttributes, usize, MarkdownText)> {
    terminated(
        tuple((
            opt(terminated(parse_attributes, tag("\n"))),
            parse_header_tag,
            parse_markdown_text(false),
        )),
        block_ending,
    )(i.trim_start())
}

fn parse_unordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(tag("-"), tag(" "))(i)
}

fn parse_unordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    preceded(parse_unordered_list_tag, parse_markdown_text(false))(i)
}

fn parse_unordered_list(i: &str) -> IResult<&str, (MarkdownAttributes, Vec<MarkdownText>)> {
    pair(
        opt(terminated(parse_attributes, tag("\n"))),
        many1(parse_unordered_list_element),
    )(i.trim_start())
}

fn parse_ordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(
        terminated(take_while1(|d| is_digit(d as u8)), tag(".")),
        tag(" "),
    )(i)
}

fn parse_ordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    delimited(
        parse_ordered_list_tag,
        parse_markdown_text(false),
        block_ending,
    )(i)
}

fn parse_ordered_list(i: &str) -> IResult<&str, (MarkdownAttributes, Vec<MarkdownText>)> {
    pair(
        opt(terminated(parse_attributes, tag("\n"))),
        many1(parse_ordered_list_element),
    )(i.trim_start())
}

fn parse_code_block(i: &str) -> IResult<&str, ((&str, MarkdownAttributes), &str)> {
    tuple((parse_code_block_lang, parse_code_block_body))(i.trim_start())
}

fn parse_code_block_body(i: &str) -> IResult<&str, &str> {
    delimited(tag("\n"), is_not("```"), pair(tag("```"), block_ending))(i)
}

fn parse_code_block_lang(i: &str) -> IResult<&str, (&str, MarkdownAttributes)> {
    alt((
        preceded(tag("```"), parse_plaintext(true)),
        map(tag("```"), |_| ("__UNKNOWN__", None)),
    ))(i)
}

fn parse_cell(input: &str) -> IResult<&str, MarkdownText> {
    map(
        delimited(
            space0,
            verify(take_till(|c| c == '|'), |s: &str| !s.trim().is_empty()),
            space0,
        ),
        |s: &str| {
            parse_markdown_text(false)(s.trim()).unwrap_or_default().1 // Trim the text to remove leading/trailing spaces
        },
    )(input)
}

// fn parse_row<'a, O, F>(cell_parser: F) -> impl Fn(&'a str) -> IResult<&'a str, Vec<O>> + 'a
// where
//     F: Fn(&'a str) -> IResult<&'a str, O> + 'a,
// {
//     move |input: &'a str| {
//         delimited(
//             char('|'),
//             separated_list1(char('|'), &cell_parser),
//             // cut(terminated(multispace0, char('|')))
//             preceded(space0, char('|')),
//         )(input.trim())
//     }
// }
// fn parse_row<'a, O, F>(cell_parser: F) -> impl Fn(&'a str) -> IResult<&'a str, Vec<O>> + 'a
// where
//     F: Fn(&'a str) -> IResult<&'a str, O> + 'a,
// {
//     move |input: &'a str| {
//         delimited(
//             char('|'),
//             separated_list1(char('|'), &cell_parser),
//             delimited(space0, char('|'), peek(char('\n'))),
//         )(input.trim_end())
//     }
// }
fn parse_row<'a, O, F>(cell_parser: F) -> impl Fn(&'a str) -> IResult<&'a str, Vec<O>> + 'a
where
    F: Fn(&'a str) -> IResult<&'a str, O> + 'a,
    O: std::fmt::Debug,
{
    move |input: &'a str| {
        let (input, _) = char('|')(input)?;
        let (input, cells) = separated_list1(char('|'), &cell_parser)(input)?;
        let (input, matched) = opt(pair(char('|'), opt(peek(char('\n')))))(input)?;
        Ok((input, cells))
    }
}

fn parse_alignment(input: &str) -> IResult<&str, Alignment> {
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::NonEmpty,
        )));
    }
    map(
        tuple((
            space0,
            opt(char::<&str, nom::error::Error<&str>>(':')),
            recognize(many1(char('-'))),
            opt(char::<&str, nom::error::Error<&str>>(':')),
            space0,
        )),
        |(_, left, _, right, _)| match (left, right) {
            (Some(_), Some(_)) => Alignment::Center,
            (Some(_), None) => Alignment::Left,
            (None, Some(_)) => Alignment::Right,
            _ => Alignment::Left,
        },
    )(input)
}

fn parse_table(
    input: &str,
) -> IResult<&str, (Vec<MarkdownText>, Vec<Alignment>, Vec<Vec<MarkdownText>>)> {
    map(
        tuple((
            parse_row(parse_cell),
            char('\n'),
            parse_row(parse_alignment),
            char('\n'),
            separated_list1(char('\n'), parse_row(parse_cell)),
        )),
        |(headers, _, alignments, _, rows)| (headers, alignments, rows),
    )(input.trim())
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{error::Error, error::ErrorKind, Err as NomErr};

    #[test]
    fn test_parse_attributes() {
        assert_eq!(
            parse_attributes("{ id=\"abc\" b=\"c\" }"),
            Ok((
                "",
                HashMap::from([("id".into(), "abc".into()), ("b".into(), "c".into())])
            ))
        );

        assert_eq!(
            parse_attributes("{#idName .class1 .class2 b=\"c\" }"),
            Ok((
                "",
                HashMap::from([
                    ("id".into(), "idName".into()),
                    ("class".into(), "class1 class2".into()),
                    ("b".into(), "c".into())
                ])
            ))
        );

        assert_eq!(
            parse_attributes("{#idName .class1 .class2 id=\"abc\" b=\"c\" }"),
            Ok((
                "",
                HashMap::from([
                    ("b".into(), "c".into()),
                    ("id".into(), "idName abc".into()),
                    ("class".into(), "class1 class2".into())
                ])
            ))
        )
    }

    #[test]
    fn test_surround_with_attrs() {
        assert_eq!(
            match_surround2_with_attrs(true, "*", "*")("*here is italic*"),
            Ok((
                "",
                (
                    vec![MarkdownInline::Plaintext("here is italic", None)],
                    None
                )
            ))
        );
        assert_eq!(
            match_surround2_with_attrs(true, "*", "*")("*here is italic*{ id=\"abc\" b=\"c\" }"),
            Ok((
                "",
                (
                    vec![MarkdownInline::Plaintext("here is italic", None)],
                    Some(HashMap::from([
                        ("id".into(), "abc".into()),
                        ("b".into(), "c".into())
                    ]))
                )
            ))
        );
    }

    #[test]
    fn test_parse_markdown_inline() {
        assert_eq!(
            parse_markdown_inline(false)("here is *italic*"),
            Ok(("*italic*", MarkdownInline::Plaintext("here is ", None)))
        );
        // assert_eq!(
        //     parse_markdown_inline(false)("*here is italic*"),
        //     Ok(("", MarkdownInline::Italic("here is italic", None)))
        // );
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
            Ok((
                "",
                MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("here is italic", None)],
                    Some(HashMap::from([
                        ("id".into(), "abc".into()),
                        ("b".into(), "c".into())
                    ]))
                )
            ))
        );
        assert_eq!(
            parse_markdown_inline(false)("*here is \nitalic*"),
            Ok(("\nitalic*", MarkdownInline::Plaintext("*here is ", None)))
        )
    }

    #[test]
    fn test_parse_markdown_text() {
        assert_eq!(
            parse_markdown_not_plain(false)("*here is strong*"),
            Ok((
                "",
                MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("here is strong", None)],
                    None
                )
            ))
        );

        assert_eq!(
            parse_plaintext(false)("*here is \nitalic*"),
            Ok(("\nitalic*", ("*here is ", None)))
        );
        assert_eq!(
            parse_plaintext(true)(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is bold*"),
            Ok((
                "",
                vec![MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("here is bold", None)],
                    None
                )]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic* lorem ipsum"),
            Ok((
                "",
                vec![
                    MarkdownInline::Bold(
                        vec![MarkdownInline::Plaintext("here is italic", None)],
                        None
                    ),
                    MarkdownInline::Plaintext(" lorem ipsum", None)
                ]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here*italic"),
            Ok((
                "",
                vec![
                    MarkdownInline::Bold(vec![MarkdownInline::Plaintext("here", None)], None),
                    MarkdownInline::Plaintext("italic", None)
                ]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic* lorem ipsum"),
            Ok((
                "",
                vec![
                    MarkdownInline::Bold(
                        vec![MarkdownInline::Plaintext("here is italic", None)],
                        None
                    ),
                    MarkdownInline::Plaintext(" lorem ipsum", None)
                ]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic * lorem ipsum"),
            Ok((
                "",
                vec![MarkdownInline::Plaintext(
                    "*here is italic * lorem ipsum",
                    None
                )]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic*{ id=\"abc\" b=\"c\" }"),
            Ok((
                "",
                vec![MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("here is italic", None)],
                    Some(HashMap::from([
                        ("id".into(), "abc".into()),
                        ("b".into(), "c".into())
                    ]))
                )]
            ))
        );
        assert_eq!(
            parse_markdown_text(true)("*here is italic*{ .className }"),
            Ok((
                "",
                vec![MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("here is italic", None)],
                    Some(HashMap::from([("class".into(), "className".into())]))
                )]
            ))
        );
    }

    #[test]
    fn test_parse_markdown_block() {
        assert_eq!(
            parse_markdown("*here*italics"),
            Ok((
                "",
                vec![Markdown::Line(
                    vec![
                        MarkdownInline::Bold(vec![MarkdownInline::Plaintext("here", None)], None),
                        MarkdownInline::Plaintext("italics", None)
                    ],
                    None
                )]
            ))
        );
        assert_eq!(
            parse_markdown("foo*bar*"),
            Ok((
                "",
                vec![Markdown::Line(
                    vec![
                        MarkdownInline::Plaintext("foo", None),
                        MarkdownInline::Bold(vec![MarkdownInline::Plaintext("bar", None)], None)
                    ],
                    None
                )]
            ))
        );
        assert_eq!(
            parse_header("# The header"),
            Ok((
                "",
                (None, 1, vec![MarkdownInline::Plaintext("The header", None)])
            ))
        );
        // assert_eq!(
        //     parse_header("# The \nheader"),
        //     Ok((
        //         "",
        //         (
        //             None,
        //             1,
        //             vec![MarkdownInline::Plaintext("The \nheader", None)]
        //         )
        //     ))
        // );
        assert_eq!(
            parse_markdown("## foo*bar*"),
            Ok((
                "",
                vec![Markdown::Heading(
                    2,
                    vec![
                        MarkdownInline::Plaintext("foo", None),
                        MarkdownInline::Bold(vec![MarkdownInline::Plaintext("bar", None)], None),
                    ],
                    Some(HashMap::from([("id".into(), "foobar".into())]))
                ),]
            ))
        );
        assert_eq!(
            parse_markdown("{.className}\n## foo*bar* "),
            Ok((
                "",
                vec![Markdown::Heading(
                    2,
                    vec![
                        MarkdownInline::Plaintext("foo", None),
                        MarkdownInline::Bold(vec![MarkdownInline::Plaintext("bar", None)], None),
                        MarkdownInline::Plaintext(" ", None)
                    ],
                    Some(HashMap::from([
                        ("class".into(), "className".into()),
                        ("id".into(), "foobar".into())
                    ]))
                ),]
            ))
        );
        assert_eq!(
            parse_markdown("## foo*bar*\n\nParagraph trial"),
            Ok((
                "",
                vec![
                    Markdown::Heading(
                        2,
                        vec![
                            MarkdownInline::Plaintext("foo", None),
                            MarkdownInline::Bold(
                                vec![MarkdownInline::Plaintext("bar", None)],
                                None
                            ),
                        ],
                        Some(HashMap::from([("id".into(), "foobar".into())]))
                    ),
                    Markdown::Line(
                        vec![MarkdownInline::Plaintext("Paragraph trial", None)],
                        None
                    )
                ]
            ))
        );
        assert_eq!(
            parse_markdown("## foo*bar*\n\nParagraph trial\n\nNew Paragraph"),
            Ok((
                "",
                vec![
                    Markdown::Heading(
                        2,
                        vec![
                            MarkdownInline::Plaintext("foo", None),
                            MarkdownInline::Bold(
                                vec![MarkdownInline::Plaintext("bar", None)],
                                None
                            ),
                        ],
                        Some(HashMap::from([("id".into(), "foobar".into())]))
                    ),
                    Markdown::Line(
                        vec![MarkdownInline::Plaintext("Paragraph trial", None)],
                        None
                    ),
                    Markdown::Line(vec![MarkdownInline::Plaintext("New Paragraph", None)], None)
                ]
            ))
        );
        assert_eq!(
            parse_markdown(
                "{.className}\n## foo*bar*\n\nParagraph trial\n![](https://placehold.it/200x200)\n"
            ),
            Ok((
                "",
                vec![
                    Markdown::Heading(
                        2,
                        vec![
                            MarkdownInline::Plaintext("foo", None),
                            MarkdownInline::Bold(
                                vec![MarkdownInline::Plaintext("bar", None)],
                                None
                            ),
                        ],
                        Some(HashMap::from([
                            ("class".into(), "className".into()),
                            ("id".into(), "foobar".into())
                        ]))
                    ),
                    Markdown::Line(
                        vec![
                            MarkdownInline::Plaintext("Paragraph trial", None),
                            MarkdownInline::LineBreak,
                            MarkdownInline::Image("", "https://placehold.it/200x200", None),
                        ],
                        None
                    ),
                ]
            ))
        );
        assert_eq!(
            parse_markdown(
                "## foo*bar*\n\n{.className}\nParagraph trial\n![](https://placehold.it/200x200)"
            ),
            Ok((
                "",
                vec![
                    Markdown::Heading(
                        2,
                        vec![
                            MarkdownInline::Plaintext("foo", None),
                            MarkdownInline::Bold(
                                vec![MarkdownInline::Plaintext("bar", None)],
                                None
                            ),
                        ],
                        Some(HashMap::from([("id".into(), "foobar".into())]))
                    ),
                    Markdown::Line(
                        vec![
                            MarkdownInline::Plaintext("Paragraph trial", None),
                            MarkdownInline::LineBreak,
                            MarkdownInline::Image("", "https://placehold.it/200x200", None)
                        ],
                        Some(HashMap::from([("class".into(), "className".into())]))
                    ),
                ]
            ))
        );
    }


    #[test]
    fn test_parse_markdown_document2() {
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
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("[link babey](and then somewhat)"),
            Err(NomErr::Error(Error {
                input: "[link babey](and then somewhat)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("`codeblock for bums`"),
            Err(NomErr::Error(Error {
                input: "`codeblock for bums`",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("![ but wait theres more](jk)"),
            Err(NomErr::Error(Error {
                input: "![ but wait theres more](jk)",
                code: ErrorKind::Not
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
            parse_plaintext(true)("here is plaintext"),
            Ok(("", ("here is plaintext", None)))
        );
        assert_eq!(
            parse_plaintext(true)("*here is italic*"),
            Err(NomErr::Error(Error {
                input: "*here is italic*",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("**here is bold**"),
            Err(NomErr::Error(Error {
                input: "**here is bold**",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("`here is code`"),
            Err(NomErr::Error(Error {
                input: "`here is code`",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("[title](https://www.example.com)"),
            Err(NomErr::Error(Error {
                input: "[title](https://www.example.com)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(true)("![alt text](image.jpg)"),
            Err(NomErr::Error(Error {
                input: "![alt text](image.jpg)",
                code: ErrorKind::Not
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

    #[test]
    fn test_parse_table() {
        env_logger::init();
        // assert_eq!(
        //     parse_row(parse_cell)("| a | b | c |"),
        //     Ok((
        //         "",
        //         vec![
        //             vec![MarkdownInline::Plaintext("a", None)],
        //             vec![MarkdownInline::Plaintext("b", None)],
        //             vec![MarkdownInline::Plaintext("c", None)],
        //         ]
        //     ))
        // );
        // assert_eq!(parse_alignment("--------"), Ok(("", Alignment::Left)));
        // assert_eq!(parse_alignment(":--------"), Ok(("", Alignment::Left)));
        // assert_eq!(parse_alignment("--------:"), Ok(("", Alignment::Right)));
        // assert_eq!(parse_alignment(":--------:"), Ok(("", Alignment::Center)));
        // assert_eq!(parse_alignment(" :--------: "), Ok(("", Alignment::Center)));

        // assert_eq!(
        //     parse_row(parse_alignment)("| -------- | -------: | :-----------: |"),
        //     Ok((
        //         "",
        //         vec![Alignment::Left, Alignment::Right, Alignment::Center]
        //     ))
        // );

        assert_eq!(
            parse_row(parse_alignment)("| -------- | -------: |\n| :-----------: |\n"),
            Ok((
                "\n| :-----------: |\n",
                vec![Alignment::Left, Alignment::Right]
            ))
        );

        assert_eq!(
            separated_list1(char('\n'), parse_row(parse_cell))("| a | b |\n| c | d |"),
            Ok((
                "",
                vec![
                    vec![
                        vec![MarkdownInline::Plaintext("a", None)],
                        vec![MarkdownInline::Plaintext("b", None)],
                    ],
                    vec![
                        vec![MarkdownInline::Plaintext("c", None)],
                        vec![MarkdownInline::Plaintext("d", None)]
                    ]
                ]
            ))
        );

        let md_val = r#"| Material | Quantity | Catch-phrase  |
| -------- | -------: | :-----------: |
| cotton   |       42 |   Practical!  |
| wool     |       17 |     Warm!     |
| silk     |        4 |    Smooth!    |"#;
        assert_eq!(
            parse_table(md_val),
            Ok((
                "",
                (
                    vec![
                        vec![MarkdownInline::Plaintext("Material", None)],
                        vec![MarkdownInline::Plaintext("Quantity", None)],
                        vec![MarkdownInline::Plaintext("Catch-phrase", None)],
                    ],
                    vec![Alignment::Left, Alignment::Right, Alignment::Center],
                    vec![
                        vec![
                            vec![MarkdownInline::Plaintext("cotton", None)],
                            vec![MarkdownInline::Plaintext("42", None)],
                            vec![MarkdownInline::Plaintext("Practical!", None)],
                        ],
                        vec![
                            vec![MarkdownInline::Plaintext("wool", None)],
                            vec![MarkdownInline::Plaintext("17", None)],
                            vec![MarkdownInline::Plaintext("Warm!", None)],
                        ],
                        vec![
                            vec![MarkdownInline::Plaintext("silk", None)],
                            vec![MarkdownInline::Plaintext("4", None)],
                            vec![MarkdownInline::Plaintext("Smooth!", None)],
                        ],
                    ],
                ),
            )),
        )
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
