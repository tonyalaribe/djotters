use std::collections::HashMap;

use crate::Markdown;
use crate::MarkdownInline;
use crate::MarkdownText;

use nom::{sequence::separated_pair, bytes::complete::{take_until, take_till}};
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
        map(parse_header, |e| Markdown::Heading(e.0, e.1, e.2)),
        map(parse_unordered_list, |e| Markdown::UnorderedList(e)),
        map(parse_ordered_list, |e,attr| Markdown::OrderedList(e)),
        map(parse_code_block, |e| {
            Markdown::Codeblock(e.0.to_string(), e.1, e.2)
        }),
        map(parse_markdown_text, |e,attr| Markdown::Line(e,attr)),
    )))(i)
}

fn parse_boldtext(i: &str) -> IResult<&str, &str> {
    delimited(tag("**"), is_not("**"), tag("**"))(i)
}

fn match_surround_text<'a>(i: &'a str, surrounder: &'a str) -> IResult<&'a str, &'a str> {
    delimited(
        pair(tag(surrounder), not(tag(" "))), // Match opening "*" and ensure no space after it
        alt((is_not(surrounder), is_not("\n"))),
        pair(not(tag(" ")), tag(surrounder)) // Ensure no space before closing "*" and match it
    )(i)
}


fn parse_attributes(input: &str) -> IResult<&str, HashMap<&str, &str>> {
    let parse_key = alphanumeric1;
    let parse_value = delimited(tag("\""), take_while(|c| c != '"'), tag("\""));

    let parse_pair = separated_pair(parse_key, tag("="), parse_value);
    let parse_pairs = separated_list0(multispace1, parse_pair);

    let mut attributes_parser = delimited(
        tag("{"),
        map(
            opt(parse_pairs),
            |pairs| pairs.unwrap_or_default().into_iter().collect::<HashMap<_, _>>(),
        ),
        tag("}"),
    );

    attributes_parser(input)
}

// fn parse_italics(input: &str) -> IResult<&str, (&str, Option<HashMap<&str, &str>>)> {
//     let (remaining, text) = match_surround_text(input, "*")?;
//     let (remaining, attrs) = opt(parse_attributes)(text)?;

//     Ok((remaining, (text, attrs)))
// }

fn parse_italics(input: &str) -> IResult<&str, (&str, Option<HashMap<&str, &str>>)> {
    let (remaining, text) = match_surround_text(input, "*")?;
    let (remaining, attrs) = opt(parse_attributes)(remaining)?;

    Ok((remaining, (text, attrs)))
}


fn parse_inline_code(i: &str) -> IResult<&str, &str> {
    delimited(tag("`"), is_not("`"), tag("`"))(i)
}

fn parse_link(i: &str) -> IResult<&str, (&str, &str)> {
    pair(
        delimited(tag("["), is_not("]"), tag("]")),
        delimited(tag("("), is_not(")"), tag(")")),
    )(i)
}

fn parse_image(i: &str) -> IResult<&str, (&str, &str)> {
    pair(
        delimited(tag("!["), is_not("]"), tag("]")),
        delimited(tag("("), is_not(")"), tag(")")),
    )(i)
}

// // we want to match many things that are not any of our specail tags
// // but since we have no tools available to match and consume in the negative case (without regex)
// // we need to match against our tags, then consume one char
// // we repeat this until we run into one of our special characters
// // then we join our array of characters into a String
// fn parse_plaintext(i: &str) -> IResult<&str, String> {
//     map(
//         many1(preceded(
//             not(alt((tag("*"), tag("`"), tag("["), tag("!["), tag("\n")))),
//             take(1u8),
//         )),
//         |vec| vec.join(""),
//     )(i)
// }

fn parse_plaintext(i: &str) -> IResult<&str, String> {
    map(
        many1(preceded(
            not(tag("\n")),
            take(1u8),
        )),
        |vec| vec.join(""),
    )(i)
}

fn parse_markdown_inline(i: &str) -> IResult<&str, MarkdownInline> {
    alt((
        map(parse_italics, |(s, attr): (&str, MarkdownAttributes)| {
            MarkdownInline::Italic(s, attr )
        }),
        map(parse_inline_code, |(s, attr): (&str, MarkdownAtributes)| {
            MarkdownInline::InlineCode(s, attr)
        }),
        map(parse_boldtext, |(s, attr): (&str, MarkdownAtributes)| {
            MarkdownInline::Bold(s, attr)
        }),
        map(parse_image, |(tag, url, attr): (&str, &str, MarkdownAtributes)| {
            MarkdownInline::Image(tag, url,attr)
        }),
        map(parse_link, |(tag, url, attr): (&str, &str, MarkdownAtributes)| {
            MarkdownInline::Link(tag, url, attr)
        }),
        map(parse_plaintext, |(s, attr): (&str, MarkdownAtributes)| MarkdownInline::Plaintext(s, attr)),
    ))(i)
}

fn parse_markdown_text(i: &str) -> IResult<&str, MarkdownText> {
    terminated(many0(parse_markdown_inline), tag("\n"))(i)
}

// this guy matches the literal character #
fn parse_header_tag(i: &str) -> IResult<&str, usize> {
    map(
        terminated(take_while1(|c| c == '#'), tag(" ")),
        |s: &str| s.len(),
    )(i)
}

// this combines a tuple of the header tag and the rest of the line
fn parse_header(i: &str) -> IResult<&str, (usize, MarkdownText, MarkdownAttri)> {
    tuple((parse_header_tag, parse_markdown_text))(i)
}

fn parse_unordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(tag("-"), tag(" "))(i)
}

fn parse_unordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    preceded(parse_unordered_list_tag, parse_markdown_text)(i)
}

fn parse_unordered_list(i: &str) -> IResult<&str, Vec<MarkdownText>> {
    many1(parse_unordered_list_element)(i)
}

fn parse_ordered_list_tag(i: &str) -> IResult<&str, &str> {
    terminated(
        terminated(take_while1(|d| is_digit(d as u8)), tag(".")),
        tag(" "),
    )(i)
}

fn parse_ordered_list_element(i: &str) -> IResult<&str, MarkdownText> {
    preceded(parse_ordered_list_tag, parse_markdown_text)(i)
}

fn parse_ordered_list(i: &str) -> IResult<&str, Vec<MarkdownText>> {
    many1(parse_ordered_list_element)(i)
}

fn parse_code_block(i: &str) -> IResult<&str, (String, &str)> {
    tuple((parse_code_block_lang, parse_code_block_body))(i)
}

fn parse_code_block_body(i: &str) -> IResult<&str, &str> {
    delimited(tag("\n"), is_not("```"), tag("```"))(i)
}

fn parse_code_block_lang(i: &str) -> IResult<&str, String> {
    alt((
        preceded(tag("```"), parse_plaintext),
        map(tag("```"), |_| "__UNKNOWN__".to_string()),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{error::Error, error::ErrorKind, Err as NomErr};

    #[test]
    fn test_parse_italics() {
        assert_eq!(
            parse_italics("*here is italic*"),
            Ok(("", ("here is italic", None)))
        );

        assert_eq!(
            parse_italics("*here is italic*{id=\"abc\"}"),
            Ok(
                ("", ("here is italic", Some(HashMap::from([("id", "abc")]))))
            )
        );

        assert_eq!(
            parse_italics("*here is italic*{a=\"b\" c=\"d\"}"),
            Ok(
                ("", ("here is italic", Some(HashMap::from([("a", "b"), ("c", "d")]))))
            )
        );

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

    #[test]
    fn test_parse_boldtext() {
        assert_eq!(parse_boldtext("**here is bold**"), Ok(("", "here is bold")));
        assert_eq!(
            parse_boldtext("**here is bold"),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_boldtext("here is bold**"),
            Err(NomErr::Error(Error {
                input: "here is bold**",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_boldtext("here is bold"),
            Err(NomErr::Error(Error {
                input: "here is bold",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_boldtext("****"),
            Err(NomErr::Error(Error {
                input: "**",
                code: ErrorKind::IsNot
            }))
        );
        assert_eq!(
            parse_boldtext("**"),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::IsNot
            }))
        );
        assert_eq!(
            parse_boldtext("*"),
            Err(NomErr::Error(Error {
                input: "*",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_boldtext(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_boldtext("*this is italic*"),
            Err(NomErr::Error(Error {
                input: "*this is italic*",
                code: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_parse_inline_code() {
        assert_eq!(
            parse_boldtext("**here is bold**\n"),
            Ok(("\n", "here is bold"))
        );
        assert_eq!(
            parse_inline_code("`here is code"),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_inline_code("here is code`"),
            Err(NomErr::Error(Error {
                input: "here is code`",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            parse_inline_code("``"),
            Err(NomErr::Error(Error {
                input: "`",
                code: ErrorKind::IsNot
            }))
        );
        assert_eq!(
            parse_inline_code("`"),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::IsNot
            }))
        );
        assert_eq!(
            parse_inline_code(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_parse_link() {
        assert_eq!(
            parse_link("[title](https://www.example.com)"),
            Ok(("", ("title", "https://www.example.com")))
        );
        assert_eq!(
            parse_inline_code(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_parse_image() {
        assert_eq!(
            parse_image("![alt text](image.jpg)"),
            Ok(("", ("alt text", "image.jpg")))
        );
        assert_eq!(
            parse_inline_code(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_parse_plaintext() {
        assert_eq!(
            parse_plaintext("1234567890"),
            Ok(("", String::from("1234567890")))
        );
        assert_eq!(
            parse_plaintext("oh my gosh!"),
            Ok(("", String::from("oh my gosh!")))
        );
        assert_eq!(
            parse_plaintext("oh my gosh!["),
            Ok(("![", String::from("oh my gosh")))
        );
        assert_eq!(
            parse_plaintext("oh my gosh!*"),
            Ok(("*", String::from("oh my gosh!")))
        );
        assert_eq!(
            parse_plaintext("*bold babey bold*"),
            Err(NomErr::Error(Error {
                input: "*bold babey bold*",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("[link babey](and then somewhat)"),
            Err(NomErr::Error(Error {
                input: "[link babey](and then somewhat)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("`codeblock for bums`"),
            Err(NomErr::Error(Error {
                input: "`codeblock for bums`",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("![ but wait theres more](jk)"),
            Err(NomErr::Error(Error {
                input: "![ but wait theres more](jk)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("here is plaintext"),
            Ok(("", String::from("here is plaintext")))
        );
        assert_eq!(
            parse_plaintext("here is plaintext!"),
            Ok(("", String::from("here is plaintext!")))
        );
        assert_eq!(
            parse_plaintext("here is plaintext![image starting"),
            Ok(("![image starting", String::from("here is plaintext")))
        );
        assert_eq!(
            parse_plaintext("here is plaintext\n"),
            Ok(("\n", String::from("here is plaintext")))
        );
        assert_eq!(
            parse_plaintext("*here is italic*"),
            Err(NomErr::Error(Error {
                input: "*here is italic*",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("**here is bold**"),
            Err(NomErr::Error(Error {
                input: "**here is bold**",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("`here is code`"),
            Err(NomErr::Error(Error {
                input: "`here is code`",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("[title](https://www.example.com)"),
            Err(NomErr::Error(Error {
                input: "[title](https://www.example.com)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext("![alt text](image.jpg)"),
            Err(NomErr::Error(Error {
                input: "![alt text](image.jpg)",
                code: ErrorKind::Not
            }))
        );
        assert_eq!(
            parse_plaintext(""),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Eof
            }))
        );
    }

    #[test]
    fn test_parse_markdown_inline() {
        assert_eq!(
            parse_markdown_inline("*here is italic*"),
            Ok(("", MarkdownInline::Italic("here is italic", None)))
        );
        assert_eq!(
            parse_markdown_inline("**here is bold**"),
            Ok(("", MarkdownInline::Bold("here is bold", None)))
        );
        assert_eq!(
            parse_markdown_inline("`here is code`"),
            Ok(("", MarkdownInline::InlineCode("here is code", None)))
        );
        assert_eq!(
            parse_markdown_inline("[title](https://www.example.com)"),
            Ok((
                "",
                (MarkdownInline::Link(
                    "title",
                    "https://www.example.com",
                    None
                ))
            ))
        );
        assert_eq!(
            parse_markdown_inline("![alt text](image.jpg)"),
            Ok((
                "",
                (MarkdownInline::Image("alt text", "image.jpg", None))
            ))
        );
        assert_eq!(
            parse_markdown_inline("here is plaintext!"),
            Ok((
                "",
                MarkdownInline::Plaintext("here is plaintext!", None)
            ))
        );
        assert_eq!(
            parse_markdown_inline("here is some plaintext *but what if we italicize?"),
            Ok((
                "*but what if we italicize?",
                MarkdownInline::Plaintext("here is some plaintext ", None)
            ))
        );
        assert_eq!(
            parse_markdown_inline(
                r#"here is some plaintext 
*but what if we italicize?"#
            ),
            Ok((
                "\n*but what if we italicize?",
                MarkdownInline::Plaintext("here is some plaintext ", None)
            ))
        );
        // assert_eq!(
        //     parse_markdown_inline("\n"),
        //     Err(NomErr::Error(Error {
        //         input: "\n",
        //         code: ErrorKind::Not
        //     }))
        // );
        // assert_eq!(
        //     parse_markdown_inline(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Eof
        //     }))
        // );
    }

    #[test]
    fn test_parse_markdown_text() {
        assert_eq!(parse_markdown_text("\n"), Ok(("", vec![])));
        assert_eq!(
            parse_markdown_text("here is some plaintext\n"),
            Ok((
                "",
                vec![MarkdownInline::Plaintext("here is some plaintext", None)]
            ))
        );
        assert_eq!(
            parse_markdown_text("here is some plaintext *but what if we italicize?*\n"),
            Ok((
                "",
                vec![
                    MarkdownInline::Plaintext("here is some plaintext ", None),
                    MarkdownInline::Italic("but what if we italicize?", None),
                ]
            ))
        );
        assert_eq!(
            parse_markdown_text("here is some plaintext *but what if we italicize?* I guess it doesnt **matter** in my `code`\n"),
            Ok(("", vec![
                MarkdownInline::Plaintext("here is some plaintext ", None),
                MarkdownInline::Italic("but what if we italicize?", None),
                MarkdownInline::Plaintext(" I guess it doesnt ", None),
                MarkdownInline::Bold("matter", None),
                MarkdownInline::Plaintext(" in my ", None),
                MarkdownInline::InlineCode("code", None),
            ]))
        );
        assert_eq!(
            parse_markdown_text("here is some plaintext *but what if we italicize?*\n"),
            Ok((
                "",
                vec![
                    MarkdownInline::Plaintext("here is some plaintext ", None),
                    MarkdownInline::Italic("but what if we italicize?", None),
                ]
            ))
        );
        // assert_eq!(
        //     parse_markdown_text("here is some plaintext *but what if we italicize?"),
        //     Err(NomErr::Error(Error {
        //         input: "*but what if we italicize?",
        //         code: ErrorKind::Tag
        //     })) // Ok(("*but what if we italicize?", vec![MarkdownInline::Plaintext(String::from("here is some plaintext "))]))
        // );
    }

    #[test]
    fn test_parse_header_tag() {
        assert_eq!(parse_header_tag("# "), Ok(("", 1)));
        assert_eq!(parse_header_tag("### "), Ok(("", 3)));
        assert_eq!(parse_header_tag("# h1"), Ok(("h1", 1)));
        assert_eq!(parse_header_tag("# h1"), Ok(("h1", 1)));
        assert_eq!(
            parse_header_tag(" "),
            Err(NomErr::Error(Error {
                input: " ",
                code: ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(
            parse_header_tag("#"),
            Err(NomErr::Error(Error {
                input: "",
                code: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_parse_header() {
        assert_eq!(
            parse_header("# h1\n"),
            Ok(("", (1, vec![MarkdownInline::Plaintext("h1", None)])))
        );
        assert_eq!(
            parse_header("## h2\n"),
            Ok(("", (2, vec![MarkdownInline::Plaintext("h2", None)])))
        );
        assert_eq!(
            parse_header("###  h3\n"),
            Ok((
                "",
                (3, vec![MarkdownInline::Plaintext(" h3", None)])
            ))
        );
        // assert_eq!(
        //     parse_header("###h3"),
        //     Err(NomErr::Error(Error {
        //         input: "h3",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_header("###"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_header(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::TakeWhile1
        //     }))
        // );
        // assert_eq!(
        //     parse_header("#"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(parse_header("# \n"), Ok(("", (1, vec![]))));
        // assert_eq!(
        //     parse_header("# test"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
    }

    #[test]
    fn test_parse_unordered_list_tag() {
        assert_eq!(parse_unordered_list_tag("- "), Ok(("", "-")));
        assert_eq!(
            parse_unordered_list_tag("- and some more"),
            Ok(("and some more", "-"))
        );
        // assert_eq!(
        //     parse_unordered_list_tag("-"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_unordered_list_tag("-and some more"),
        //     Err(NomErr::Error(Error {
        //         input: "and some more",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_unordered_list_tag("--"),
        //     Err(NomErr::Error(Error {
        //         input: "-",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_unordered_list_tag(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
    }

    #[test]
    fn test_parse_unordered_list_element() {
        assert_eq!(
            parse_unordered_list_element("- this is an element\n"),
            Ok((
                "",
                vec![MarkdownInline::Plaintext("this is an element", None)]
            ))
        );
        assert_eq!(
            parse_unordered_list_element(
                r#"- this is an element
- this is another element
"#
            ),
            Ok((
                "- this is another element\n",
                vec![MarkdownInline::Plaintext("this is an element", None)]
            ))
        );
        // assert_eq!(
        //     parse_unordered_list_element(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(parse_unordered_list_element("- \n"), Ok(("", vec![])));
        // assert_eq!(
        //     parse_unordered_list_element("- "),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_unordered_list_element("- test"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_unordered_list_element("-"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
    }

    #[test]
    fn test_parse_unordered_list() {
        // assert_eq!(
        //     parse_unordered_list("- this is an element"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        assert_eq!(
            parse_unordered_list("- this is an element\n"),
            Ok((
                "",vec![vec![MarkdownInline::Plaintext("this is an element", None)]], None
            ))
        );
        assert_eq!(
            parse_unordered_list(
                r#"- this is an element
- here is another
"#
            ),
            Ok((
                "",
                vec![
                    vec![MarkdownInline::Plaintext("this is an element", None)],
                    vec![MarkdownInline::Plaintext("here is another", None)]
                ],
                None
            ))
        );
    }

    #[test]
    fn test_parse_ordered_list_tag() {
        assert_eq!(parse_ordered_list_tag("1. "), Ok(("", "1")));
        assert_eq!(parse_ordered_list_tag("1234567. "), Ok(("", "1234567")));
        assert_eq!(
            parse_ordered_list_tag("3. and some more"),
            Ok(("and some more", "3"))
        );
        // assert_eq!(
        //     parse_ordered_list_tag("1"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_tag("1.and some more"),
        //     Err(NomErr::Error(Error {
        //         input: "and some more",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_tag("1111."),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_tag(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::TakeWhile1
        //     }))
        // );
    }

    #[test]
    fn test_parse_ordered_list_element() {
        assert_eq!(
            parse_ordered_list_element("1. this is an element\n"),
            Ok((
                "",
                vec![MarkdownInline::Plaintext("this is an element", None)]
            ))
        );
        assert_eq!(
            parse_ordered_list_element(
                r#"1. this is an element
1. here is another
"#
            ),
            Ok((
                "1. here is another\n",
                vec![MarkdownInline::Plaintext("this is an element", None)]
            ))
        );
        // assert_eq!(
        //     parse_ordered_list_element(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::TakeWhile1
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_element(""),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::TakeWhile1
        //     }))
        // );
        // assert_eq!(parse_ordered_list_element("1. \n"), Ok(("", vec![])));
        // assert_eq!(
        //     parse_ordered_list_element("1. test"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_element("1. "),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        // assert_eq!(
        //     parse_ordered_list_element("1."),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
    }

    #[test]
    fn test_parse_ordered_list() {
        assert_eq!(
            parse_ordered_list("1. this is an element\n"),
            Ok((
                "",
                vec![vec![MarkdownInline::Plaintext("this is an element", None)]]
            ))
        );
        // assert_eq!(
        //     parse_ordered_list("1. test"),
        //     Err(NomErr::Error(Error {
        //         input: "",
        //         code: ErrorKind::Tag
        //     }))
        // );
        assert_eq!(
            parse_ordered_list(
                r#"1. this is an element
2. here is another
"#
            ),
            Ok((
                "",
                vec![
                    vec!(MarkdownInline::Plaintext("this is an element", None)),
                    vec![MarkdownInline::Plaintext("here is another", None)]
                ]
            ))
        );
    }

    #[test]
    fn test_parse_codeblock() {
        assert_eq!(
            parse_code_block(
                r#"```bash
pip install foobar
```"#
            ),
            Ok((
                "",
                (
                    "bash",
                    r#"pip install foobar
"#, 
                    None
                )
            ))
        );
        assert_eq!(
            parse_code_block(
                r#"```python
import foobar

foobar.pluralize('word') # returns 'words'
foobar.pluralize('goose') # returns 'geese'
foobar.singularize('phenomena') # returns 'phenomenon'
```"#
            ),
            Ok((
                "",
                (
                    String::from("python"),
                    r#"import foobar

foobar.pluralize('word') # returns 'words'
foobar.pluralize('goose') # returns 'geese'
foobar.singularize('phenomena') # returns 'phenomenon'
"#
                )
            ))
        );
        // assert_eq!(
        // 	parse_code_block("```bash\n pip `install` foobar\n```"),
        // 	Ok(("", "bash\n pip `install` foobar\n"))
        // );
    }

    #[test]
    fn test_parse_codeblock_no_language() {
        assert_eq!(
            parse_code_block(
                r#"```
pip install foobar
```"#
            ),
            Ok((
                "",
                (
                    String::from("__UNKNOWN__"),
                    r#"pip install foobar
"#
                )
            ))
        );
    }

    #[test]
    fn test_parse_markdown() {
        assert_eq!(
            parse_markdown(
                r#"# Foobar

Foobar is a Python library for dealing with word pluralization.

```bash
pip install foobar
```
## Installation

Use the package manager [pip](https://pip.pypa.io/en/stable/) to install foobar.
```python
import foobar

foobar.pluralize('word') # returns 'words'
foobar.pluralize('goose') # returns 'geese'
foobar.singularize('phenomena') # returns 'phenomenon'
```"#
            ),
            Ok((
                "",
                vec![
                    Markdown::Heading(1, vec![MarkdownInline::Plaintext("Foobar", None)], None),
                    Markdown::Line(vec![], None),
                    Markdown::Line(vec![MarkdownInline::Plaintext(
                        "Foobar is a Python library for dealing with word pluralization.",
                        None
                    )], None),
                    Markdown::Line(vec![], None),
                    Markdown::Codeblock("bash", "pip install foobar\n", None),
                    Markdown::Line(vec![], None),
                    Markdown::Heading(
                        2,
                        vec![MarkdownInline::Plaintext("Installation", None)], 
                        None
                    ),
                    Markdown::Line(vec![], None),
                    Markdown::Line(vec![
                        MarkdownInline::Plaintext("Use the package manager ", None),
                        MarkdownInline::Link(
                            "pip",
                            "https://pip.pypa.io/en/stable/",
                            None
                        ),
                        MarkdownInline::Plaintext(" to install foobar.", None),
                    ], None),
                    Markdown::Codeblock(
                        "python",
                        
                            r#"import foobar

foobar.pluralize('word') # returns 'words'
foobar.pluralize('goose') # returns 'geese'
foobar.singularize('phenomena') # returns 'phenomenon'
"#, 
                            None
                        
                    ),
                ]
            ))
        )
    }
}
