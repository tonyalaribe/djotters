extern crate slugify;
use crate::MarkdownInline;
use crate::MarkdownText;
use crate::{translator, Alignment};
use crate::{Markdown, MarkdownAttributes};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_until, take_while, take_while1},
    character::complete::{
        alphanumeric1, anychar, char, line_ending, multispace0, multispace1, newline,
        not_line_ending, space0, space1,
    },
    combinator::{eof, map, map_res, not, opt, peek, recognize, rest, value, verify},
    multi::{many0, many1, many_till, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use slugify::slugify;
use std::collections::HashMap;

pub fn parse_markdown<'a>(i: &'a str) -> IResult<&'a str, Vec<Markdown>> {
    many1(alt((
        // check blockquote first before other block items, since they might be contained in a
        // blockquote
        map(parse_blockquote, |e| Markdown::BlockQuotes(e, None)),
        // Should be replaced. Hard linebreak is \<newline>
        map(tag("\n\n"), |_e| Markdown::LineBreak),
        map(parse_header, |mut e| {
            Markdown::Heading(e.1, e.2.clone(), set_or_check_header_id(&mut e.0, e.2))
        }),
        map(parse_table, |e| Markdown::Table(e.0, e.1, e.2)),
        map(parse_unordered_list, |e| Markdown::UnorderedList(e.1, e.0)),
        map(parse_ordered_list, |e| Markdown::OrderedList(e.1, e.0)),
        map(parse_code_block, |e| {
            Markdown::Codeblock(e.0 .0.into(), e.1.into(), e.0 .1)
        }),
        map(parse_div, |e| {
            Markdown::Div(e.0.map(|v| v.into()), e.1, e.2)
        }),
        map(parse_markdown_paragraph, |e| Markdown::Line(e.1, e.0)),
    )))
    .parse(i)
}

/// Parse a djot blockquote using a declarative style.
/// The blockquote is a series of lines:
///   - The first line must begin with `>` (optionally followed by a space).
///   - Subsequent lines are either marker lines (starting with `>`)
///     or lazy continuation lines (which must not start with `>`).
pub fn parse_blockquote(i: &str) -> IResult<&str, Vec<Markdown>> {
    // The first line must be a marker line.
    let (i, first_line) = parse_marker_line(i)?;

    // Wrap the not_line_ending so that it fails if it returns an empty string.
    let non_empty_line = verify(not_line_ending, |s: &str| !s.is_empty());

    let (i, mut lines) = many0(alt((
        parse_marker_line,
        delimited(
            peek(not((tag(">"), space1))),
            non_empty_line,
            alt((line_ending, eof)),
        ),
    )))
    .parse(i)?;
    lines.insert(0, first_line);

    let joined = lines.join("\n");
    if joined == "" {
        Ok((i, vec![]))
    } else {
        let x = parse_markdown(&joined);
        println!("Error3: => {joined:?} =>{x:?}");
        // Parse the joined content as block-level markdown.
        let Ok((_rest, md)) = parse_markdown(&joined) else {
            todo!()
        };
        Ok((i, md))
    }
}

/// Parse a blockquote “marker line” that starts with “>” (optionally followed by a space)
/// and then captures the rest of the line.
fn parse_marker_line(i: &str) -> IResult<&str, &str> {
    alt((
        value("", (tag(">"), space0, alt((line_ending, eof)))),
        delimited(
            (tag(">"), space1::<&str, _>),
            not_line_ending,
            alt((line_ending, eof)),
        ),
    ))
    .parse(i)
}

pub fn set_or_check_header_id(
    attrs: &mut Option<HashMap<String, String>>,
    content: Vec<MarkdownInline>,
) -> MarkdownAttributes {
    // Check if attrs is Some and if it contains the key "id"
    if let Some(ref mut attrs_map) = attrs {
        if !attrs_map.contains_key("id") {
            // If "id" is not present, insert a new "id" with the slugified content
            let slug = slugify_md(content); // Assuming slugify_md is defined elsewhere
            attrs_map.insert("id".into(), slug);
        }
    } else {
        // If attrs is None, create a new HashMap and insert the "id"
        let mut new_attrs = HashMap::new();
        let slug = slugify_md(content); // Assuming slugify_md is defined elsewhere
        new_attrs.insert("id".into(), slug);
        *attrs = Some(new_attrs);
    }

    attrs.to_owned()
}

pub fn slugify_md(content: Vec<MarkdownInline>) -> String {
    slugify!(&translator::translate_text_raw(content))
}

pub fn block_ending(i: &str) -> IResult<&str, Vec<&str>, nom::error::Error<&str>> {
    alt((many1(line_ending), map(eof, |_| Vec::new()))).parse(i)
}

fn parse_div(i: &str) -> IResult<&str, (Option<&str>, Vec<Markdown>, MarkdownAttributes)> {
    let (input, attr) = opt(terminated(parse_attributes, tag("\n"))).parse(i)?;
    let (input, colon_count) = preceded(
        multispace0,
        map(take_while1(|c| c == ':'), |s: &str| s.len()),
    )
    .parse(input)?;

    let closing_tag = ":".repeat(colon_count);
    let (input, label) =
        terminated(opt(preceded(tag(" "), take_until("\n"))), tag("\n")).parse(input)?;

    let (input, content) =
        take_before0(|| (tag("\n"), tag(&*closing_tag), block_ending)).parse(input)?;
    // Use tuple(...) so that all parts are parsed as a unit.
    let (input, _) = tuple((tag("\n"), multispace0, tag(&*closing_tag), block_ending))(input)?;
    let (_, content_md) = parse_markdown(content)?;
    Ok((input, (label, content_md, attr)))
}

fn match_surround_text<'a>(
    i: &'a str,
    opener: &'a str,
    closer: &'a str,
) -> IResult<&'a str, &'a str> {
    let mut not_surrounder = take_while1(move |c| c != '\n' && c != closer.chars().next().unwrap());
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
    )
    .parse(i)
}

fn parse_id(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, id) = delimited(
        tag("#"),
        take_before0(|| alt((multispace1, tag("}")))),
        multispace0,
    )
    .parse(input)?;
    Ok((remaining, ("id", id)))
}

fn parse_class(input: &str) -> IResult<&str, (&str, &str)> {
    let (remaining, class) = delimited(
        tag("."),
        take_before0(|| alt((multispace1, tag("}")))),
        multispace0,
    )
    .parse(input)?;
    Ok((remaining, ("class", class)))
}

fn parse_key_value_pair(input: &str) -> IResult<&str, (&str, &str)> {
    let parse_key = alphanumeric1;
    let parse_value = delimited(tag("\""), take_while(|c| c != '"'), tag("\""));
    separated_pair(parse_key, tag("="), parse_value).parse(input)
}

fn vec_to_hashmap_concat<'a>(vec: Vec<(&'a str, &str)>) -> HashMap<String, String> {
    let mut hashmap = HashMap::new();

    for (key, value) in vec {
        hashmap
            .entry(key.into())
            .and_modify(|e| *e = format!("{} {}", e, value))
            .or_insert(value.to_string());
    }

    hashmap
}

fn parse_attributes(input: &str) -> IResult<&str, HashMap<String, String>> {
    let parse_attribute = alt((parse_id, parse_class, parse_key_value_pair));
    let parse_attributes = many0(delimited(multispace0, parse_attribute, multispace0));

    let mut attributes_parser = delimited(
        tag("{"),
        map(parse_attributes, |attrs| vec_to_hashmap_concat(attrs)),
        tag("}"),
    );
    attributes_parser.parse(input)
}

fn match_surround2_with_attrs<'a>(
    recurse: bool,
    opener: &'a str,
    closer: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, (MarkdownText, MarkdownAttributes)> {
    move |input: &'a str| {
        let (remaining, text) = match_surround_text(input, opener, closer)?;
        let (remaining, attrs) = opt(parse_attributes).parse(remaining)?;
        let (_, child_elements) = if recurse {
            parse_markdown_text(false)(text)?
        } else {
            ("", vec![MarkdownInline::Plaintext(text.into(), None)])
        };
        Ok((remaining, (child_elements, attrs)))
    }
}

fn parse_link(i: &str) -> IResult<&str, (&str, &str, MarkdownAttributes)> {
    (
        delimited(tag("["), take_until("]"), tag("]")),
        delimited(tag("("), take_until(")"), tag(")")),
        opt(parse_attributes),
    )
        .parse(i)
}

fn parse_image(i: &str) -> IResult<&str, (&str, &str, MarkdownAttributes)> {
    (
        delimited(tag("!["), take_until("]"), tag("]")),
        delimited(tag("("), take_until(")"), tag(")")),
        opt(parse_attributes),
    )
        .parse(i)
}

/// Return the remaining input.
///
/// This parser is similar to [nom::combinator::rest], but returns Err(Err::Error((_, ErrorKind::Verify))) if the input is empty.
pub fn rest1(s: &str) -> IResult<&str, &str> {
    verify(rest, |x: &str| !x.is_empty()).parse(s)
}

/// Returns the *shortest* input slice until it matches a parser.
///
/// Returns Err(Err::Error((_, ErrorKind::Eof))) if the input doesn't match the parser.
pub fn take_before0<'a, FOutput, F, G>(mut f: G) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str>
where
    G: FnMut() -> F,
    F: Parser<&'a str, Output = FOutput, Error = nom::error::Error<&'a str>>,
{
    move |input: &'a str| {
        // Call f() each time to get a fresh parser instance.
        let parser = f();
        recognize(many_till(anychar, peek(parser))).parse(input)
    }
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
            let (input, output) = take_before0(|| {
                alt((
                    parse_markdown_not_plain(accept_linebreak),
                    map(tag("\n"), |_| MarkdownInline::LineBreak),
                    map(eof, |_| MarkdownInline::LineBreak),
                ))
            })
            .parse(i)?;
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
            // ← FIX: use backticks for inline code instead of empty delimiters
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
                    MarkdownInline::Image(tag.into(), url.into(), attr)
                },
            ),
            map(tag("\\\n"), |_| MarkdownInline::LineBreak),
            map_res(terminated(tag("\n"), peek(is_not("\n"))), |_| {
                if accept_linebreak {
                    Ok(MarkdownInline::LineBreak)
                } else {
                    Err(nom::Err::<nom::error::Error<&str>>::Error(
                        nom::error::Error::new(i, nom::error::ErrorKind::Char),
                    ))
                }
            }),
            map(
                parse_link,
                |(tag, url, attr): (&str, &str, MarkdownAttributes)| {
                    MarkdownInline::Link(tag.into(), url.into(), attr)
                },
            ),
            map(
                match_surround2_with_attrs(true, "[", "]"),
                |(s, attr): (MarkdownText, MarkdownAttributes)| MarkdownInline::Span(s, attr),
            ),
        ))
        .parse(i)
    }
}

fn parse_markdown_inline(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownInline> {
    move |i: &str| {
        alt((
            parse_markdown_not_plain(accept_linebreak),
            map(
                parse_plaintext(accept_linebreak),
                |(s, attr): (&str, MarkdownAttributes)| MarkdownInline::Plaintext(s.into(), attr),
            ),
        ))
        .parse(i)
    }
}

fn parse_markdown_text(accept_linebreak: bool) -> impl Fn(&str) -> IResult<&str, MarkdownText> {
    move |i: &str| many1(parse_markdown_inline(accept_linebreak)).parse(i)
}

// A paragraph can include optional attributes, which will be on the line before it
//
// {.className #idName attr="attrVal" attr2=attrVal2}
// The paragraph content which can include *many* _inline_ {=items=}.
//

fn parse_markdown_paragraph(i: &str) -> IResult<&str, (MarkdownAttributes, MarkdownText)> {
    terminated(
        pair(
            opt(terminated(parse_attributes, tag("\n"))),
            parse_markdown_text(true),
        ),
        block_ending,
    )
    .parse(i.trim_start())
}

// this guy matches the literal character #
fn parse_header_tag(i: &str) -> IResult<&str, usize> {
    map(
        terminated(take_while1(|c| c == '#'), tag(" ")),
        |s: &str| s.len(),
    )
    .parse(i)
}

// this combines a tuple of the header tag and the rest of the line
// {.class #idName}
// # Heading title\n
//
fn parse_header(i: &str) -> IResult<&str, (MarkdownAttributes, usize, MarkdownText)> {
    terminated(
        (
            opt(terminated(parse_attributes, tag("\n"))),
            parse_header_tag,
            parse_markdown_text(false),
        ),
        block_ending,
    )
    .parse(i.trim_start())
}

fn parse_unordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(tag("-"), tag(" ")).parse(i)
}

fn parse_unordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    delimited(
        parse_unordered_list_tag,
        parse_markdown_text(false),
        block_ending,
    )
    .parse(i)
}

fn parse_unordered_list(i: &str) -> IResult<&str, (MarkdownAttributes, Vec<MarkdownText>)> {
    pair(
        opt(terminated(parse_attributes, tag("\n"))),
        many1(parse_unordered_list_element),
    )
    .parse(i.trim_start())
}

fn parse_ordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(
        terminated(take_while1(|c: char| c.is_digit(10)), tag(".")),
        tag(" "),
    )
    .parse(i)
}

fn parse_ordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    delimited(
        parse_ordered_list_tag,
        parse_markdown_text(false),
        block_ending,
    )
    .parse(i)
}

fn parse_ordered_list(i: &str) -> IResult<&str, (MarkdownAttributes, Vec<MarkdownText>)> {
    pair(
        opt(terminated(parse_attributes, tag("\n"))),
        many1(parse_ordered_list_element),
    )
    .parse(i.trim_start())
}

fn parse_code_block(i: &str) -> IResult<&str, ((&str, MarkdownAttributes), &str)> {
    terminated((parse_code_block_lang, parse_code_block_body), block_ending).parse(i.trim_start())
}

fn parse_code_block_lang(i: &str) -> IResult<&str, (&str, MarkdownAttributes)> {
    let (i, _) = tag("```")(i)?;
    let (i, lang) = opt(recognize(take_till(|c| c == '\n'))).parse(i)?;
    let (i, _) = newline(i)?;
    Ok((i, (lang.unwrap_or("__UNKNOWN__"), None)))
}

fn parse_code_block_body(i: &str) -> IResult<&str, &str> {
    let (i, body) = take_until("\n```")(i)?;
    let (i, _) = tuple((tag("\n```"), opt(newline))).parse(i)?;
    Ok((i, body))
}
// ─────────────────────────────────

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
    )
    .parse(input)
}

fn parse_row<'a, O, F>(cell_parser: F) -> impl Fn(&'a str) -> IResult<&'a str, Vec<O>> + 'a
where
    F: Fn(&'a str) -> IResult<&'a str, O> + 'a,
    O: std::fmt::Debug,
{
    move |input: &'a str| {
        let (input, _) = char('|')(input)?;
        let (input, cells) = separated_list1(char('|'), &cell_parser).parse(input)?;
        let (input, _matched) = opt(pair(char('|'), opt(peek(char('\n'))))).parse(input)?;
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
        (
            space0,
            opt(char::<&str, nom::error::Error<&str>>(':')),
            recognize(many1(char('-'))),
            opt(char::<&str, nom::error::Error<&str>>(':')),
            space0,
        ),
        |(_, left, _, right, _)| match (left, right) {
            (Some(_), Some(_)) => Alignment::Center,
            (Some(_), None) => Alignment::Left,
            (None, Some(_)) => Alignment::Right,
            _ => Alignment::Left,
        },
    )
    .parse(input)
}

fn parse_table(
    input: &str,
) -> IResult<&str, (Vec<MarkdownText>, Vec<Alignment>, Vec<Vec<MarkdownText>>)> {
    terminated(
        map(
            (
                parse_row(parse_cell),
                char('\n'),
                parse_row(parse_alignment),
                char('\n'),
                separated_list1(char('\n'), parse_row(parse_cell)),
            ),
            |(headers, _, alignments, _, rows)| (headers, alignments, rows),
        ),
        block_ending,
    )
    .parse(input.trim())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    // Helper: Join plaintext fragments from a vector of MarkdownInline.
    fn join_inlines(inlines: &[MarkdownInline]) -> String {
        inlines
            .iter()
            .map(|m| match m {
                MarkdownInline::Plaintext(s, _) => s,
                _ => "",
            })
            .collect::<String>()
    }

    // === Attributes tests ===
    #[test]
    fn attributes() {
        let cases = vec![
            (
                "{#header .title attr=\"value\"}",
                HashMap::from([
                    ("id".to_string(), "header".to_string()),
                    ("class".to_string(), "title".to_string()),
                    ("attr".to_string(), "value".to_string()),
                ]),
            ),
            (
                "{.class1 .class2}",
                HashMap::from([("class".to_string(), "class1 class2".to_string())]),
            ),
            (
                "{#id1 .class1 .class2}",
                HashMap::from([
                    ("id".to_string(), "id1".to_string()),
                    ("class".to_string(), "class1 class2".to_string()),
                ]),
            ),
        ];
        for (input, expected) in cases {
            assert_eq!(parse_attributes(input), Ok(("", expected)));
        }
    }

    // === Inline formatting tests ===
    #[test]
    fn inline_formatting() {
        // Each case: (input, expected MarkdownInline)
        let cases = vec![
            (
                "*bold*",
                MarkdownInline::Bold(vec![MarkdownInline::Plaintext("bold".into(), None)], None),
            ),
            (
                "_italic_",
                MarkdownInline::Italic(
                    vec![MarkdownInline::Plaintext("italic".into(), None)],
                    None,
                ),
            ),
            (
                "*bold*{#x}",
                MarkdownInline::Bold(
                    vec![MarkdownInline::Plaintext("bold".into(), None)],
                    Some(HashMap::from([("id".into(), "x".into())])),
                ),
            ),
        ];
        for (input, expected) in cases {
            // Here we use the parser for non–plain inlines.
            let res = parse_markdown_not_plain(false)(input);
            assert_eq!(res, Ok(("", expected)));
        }

        // Links and images use separate parsers.
        let link_cases = vec![(
            "[link](http://example.com){target=\"_blank\"}",
            MarkdownInline::Link(
                "link".into(),
                "http://example.com".into(),
                Some(HashMap::from([("target".into(), "_blank".into())])),
            ),
        )];
        for (input, expected) in link_cases {
            let res = parse_link(input)
                .map(|(_rem, (tag, url, attr))| MarkdownInline::Link(tag.into(), url.into(), attr));
            assert_eq!(res, Ok(expected));
        }

        let image_cases = vec![(
            "![alt](img.png){.img}",
            MarkdownInline::Image(
                "alt".into(),
                "img.png".into(),
                Some(HashMap::from([("class".into(), "img".into())])),
            ),
        )];
        for (input, expected) in image_cases {
            let res = parse_image(input).map(|(_rem, (alt, url, attr))| {
                MarkdownInline::Image(alt.into(), url.into(), attr)
            });
            assert_eq!(res, Ok(expected));
        }
    }

    // === Header tests ===
    #[test]
    fn headers() {
        // Each case: (input, expected_level, expected_text, expect_attrs)
        let cases = vec![
            ("# Header\n\n", 1, "Header", false),
            ("{.title}\n## Subheader\n\n", 2, "Subheader", true),
        ];
        for (input, level, text, has_attr) in cases {
            let res = parse_header(input);
            match res {
                Ok(("", (attr, lvl, inlines))) => {
                    assert_eq!(lvl, level);
                    assert_eq!(join_inlines(&inlines), text);
                    if has_attr {
                        assert!(attr.is_some());
                    } else {
                        assert!(attr.is_none());
                    }
                }
                other => panic!("Header parse failed for {:?}: {:?}", input, other),
            }
        }
    }

    // === Unordered list tests ===
    #[test]
    fn unordered_lists() {
        // Each case: (input, expected vector of list item texts)
        let cases = vec![
            (
                "- Item 1\n- Item 2\n- Item 3\n\n",
                vec!["Item 1", "Item 2", "Item 3"],
            ),
            ("- Single item\n\n", vec!["Single item"]),
        ];
        for (input, expected) in cases {
            let res = parse_unordered_list(input);
            match res {
                Ok(("", (_attr, items))) => {
                    let texts: Vec<_> = items
                        .into_iter()
                        .map(|inlines| join_inlines(&inlines))
                        .collect();
                    assert_eq!(texts, expected);
                }
                other => panic!("Unordered list parse failed for {:?}: {:?}", input, other),
            }
        }
    }

    // === Ordered list tests ===
    #[test]
    fn ordered_lists() {
        let cases = vec![
            (
                "1. First\n2. Second\n3. Third\n\n",
                vec!["First", "Second", "Third"],
            ),
            ("10. Tenth\n\n", vec!["Tenth"]),
        ];
        for (input, expected) in cases {
            let res = parse_ordered_list(input);
            match res {
                Ok(("", (_attr, items))) => {
                    let texts: Vec<_> = items
                        .into_iter()
                        .map(|inlines| join_inlines(&inlines))
                        .collect();
                    assert_eq!(texts, expected);
                }
                other => panic!("Ordered list parse failed for {:?}: {:?}", input, other),
            }
        }
    }

    // === Code block tests ===
    #[test]
    fn code_blocks() {
        // Each case: (input, expected language, expected body)
        let cases = vec![
            ("```rust\nfn main() {}\n```\n\n", "rust", "fn main() {}"),
            (
                "```python\ndef f(): pass\n```\n\n",
                "python",
                "def f(): pass",
            ),
        ];
        for (input, lang, body) in cases {
            let res = parse_code_block(input);
            match res {
                Ok(("", ((l, _attrs), b))) => {
                    assert_eq!(l, lang);
                    assert_eq!(b, body);
                }
                other => panic!("Code block parse failed for {:?}: {:?}", input, other),
            }
        }
    }

    // === Table tests ===
    #[test]
    fn tables() {
        let input = "\
| H1 | H2 |
| --:|:--:|
| A  | B  |
| C  | D  |

";
        let res = parse_table(input);
        match res {
            Ok(("", (headers, aligns, rows))) => {
                let header_text: Vec<_> = headers
                    .into_iter()
                    .map(|cell| join_inlines(&cell))
                    .collect();
                assert_eq!(header_text, vec!["H1", "H2"]);
                assert_eq!(aligns, vec![Alignment::Right, Alignment::Center]);
                let row_text: Vec<Vec<_>> = rows
                    .into_iter()
                    .map(|row| row.into_iter().map(|cell| join_inlines(&cell)).collect())
                    .collect();
                assert_eq!(row_text, vec![vec!["A", "B"], vec!["C", "D"]]);
            }
            other => panic!("Table parse failed: {:?}", other),
        }
    }

    // === Div block tests ===
    #[test]
    fn div_blocks() {
        let input = "\
{.div-class}
::: Label
Div content line 1.
Div content line 2.
:::
\n";
        let res = parse_div(input);
        match res {
            Ok(("", (label, content, maybe_attrs))) => {
                assert_eq!(label, Some("Label"));
                assert!(!content.is_empty());
                // Check that at least one line of content contains a key word.
                if let Some(Markdown::Line(inlines, _)) = content.get(0) {
                    let txt = join_inlines(inlines);
                    assert!(txt.contains("Div content"));
                } else {
                    panic!("Unexpected div content structure");
                }
                let attrs = maybe_attrs.unwrap();
                assert_eq!(attrs.get("class").map(String::as_str), Some("div-class"));
            }
            other => panic!("Div block parse failed: {:?}", other),
        }
    }

    // === Edge cases ===
    #[test]
    fn edge_cases() {
        // Empty plaintext input should error.
        assert!(parse_plaintext(true)("").is_err());

        // Unclosed markup: returns the asterisk.
        let res = parse_markdown_not_plain(false)("*unclosed bold");
        assert!(res.is_err());

        let res = parse_plaintext(false)("*unclosed bold");
        assert!(res.is_ok());

        // Stray attributes not attached to any markup should remain as plaintext.
        assert_eq!(
            parse_markdown_inline(false)("Text without markup{#id}"),
            Ok((
                "",
                MarkdownInline::Plaintext("Text without markup{#id}".into(), None)
            ))
        );
    }

    /// Test that stray attributes that are not attached to markup remain as plaintext.
    #[test]
    fn stray_attributes_in_plaintext() {
        let input = "This is a test {#test}";
        let expected = MarkdownInline::Plaintext("This is a test {#test}".into(), None);
        assert_eq!(parse_markdown_inline(false)(input), Ok(("", expected)));
    }

    /// Test that attributes attached immediately after inline markup get parsed correctly.
    #[test]
    fn inline_markup_with_attributes() {
        let input = "*bold text*{.highlight}";
        let expected = MarkdownInline::Bold(
            vec![MarkdownInline::Plaintext("bold text".into(), None)],
            Some(HashMap::from([("class".into(), "highlight".into())])),
        );
        // Here we call the non-plaintext parser so that the attribute is attached.
        assert_eq!(parse_markdown_not_plain(false)(input), Ok(("", expected)));
    }

    /// Test that a string containing multiple inline elements is parsed as a sequence.
    #[test]
    fn multiple_inlines() {
        let input = "Normal *bold* and _italic_ text.";
        let res = parse_markdown_text(false)(input);
        assert!(res.is_ok());
        let (rem, inlines) = res.unwrap();
        assert_eq!(rem, "");
        // Check that the concatenated plaintext equals the expected string
        let joined = join_inlines(&inlines);
        assert_eq!(joined, "Normal bold and italic text.");
    }

    /// Test nested inline elements.
    #[test]
    fn nested_inlines() {
        let input = "*bold with _nested italic_*";
        let res = parse_markdown_inline(false)(input);
        assert!(res.is_ok());
        let (rem, inline) = res.unwrap();
        assert_eq!(rem, "");
        if let MarkdownInline::Bold(inner, _) = inline {
            let inner_joined = join_inlines(&inner);
            assert_eq!(inner_joined, "bold with nested italic");
        } else {
            panic!("Expected a Bold inline element");
        }
    }

    /// Test that an unclosed code block returns an error.
    #[test]
    fn unclosed_code_block() {
        let input = "```rust\nfn main() {}\n";
        let res = parse_code_block(input);
        assert!(res.is_err(), "Expected an error for unclosed code block");
    }

    /// Test that a table with irregular rows is handled (or errors) appropriately.
    #[test]
    fn table_irregular_rows() {
        let input = "\
| H1 | H2 |
| --- | --- |
| A  | B  |
| C  |
\n";
        let res = parse_table(input);
        assert!(
            res.is_err(),
            "Expected an error for table with irregular row lengths"
        );
    }
}
