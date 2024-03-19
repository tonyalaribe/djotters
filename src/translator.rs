use crate::MarkdownInline;
use crate::MarkdownText;
use crate::{Markdown, MarkdownAttributes};
use itertools::Itertools;
use std::collections::HashMap;

pub fn translate(md: Vec<Markdown>) -> String {
    md.iter()
        .map(|bit| match bit {
            Markdown::Heading(size, line, attr) => translate_header(*size, line.to_vec(), attr),
            Markdown::UnorderedList(lines, attr) => translate_unordered_list(lines.to_vec(), attr),
            Markdown::OrderedList(lines, attr) => translate_ordered_list(lines.to_vec(), attr),
            Markdown::Codeblock(lang, code, attr) => translate_codeblock(lang, code, attr),
            Markdown::Line(line, attr) => translate_line(line.to_vec(), attr),
            Markdown::Div(title, content, attr) => translate_div(title, content, attr),
            Markdown::LineBreak => "\n".into(),
        })
        .collect::<Vec<String>>()
        .join("")
}

pub fn translate_div(
    title: &Option<&str>,
    content: &Vec<Markdown>,
    attr1: &MarkdownAttributes,
) -> String {
    let mut attr = attr1.clone();
    if let Some(t) = title {
        if let Some(attributes) = &mut attr {
            if let Some(val) = attributes.get_mut("class") {
                *val = format!("{} {}", val, t);
            } else {
                attributes.insert("class", t.to_string());
            }
        } else {
            let mut new_attr = HashMap::new();
            new_attr.insert("class", t.to_string());
            attr = Some(new_attr);
        }
    }

    let attr_txt = translate_attributes(&attr); // Assuming this function takes a reference
    let content_txt = translate(content.to_vec()); // Assuming this function consumes the Vec<Markdown>

    // Use format string directly without placeholder names for simplicity
    format!("<div{}>{}</div>", attr_txt, content_txt)
}

pub fn translate_attributes(opt: &MarkdownAttributes) -> String {
    opt.as_ref().map_or(String::new(), |hash_map| {
        hash_map
            .iter()
            .sorted()
            .map(|(k, v)| format!(" {}=\"{}\"", k, v))
            .collect::<Vec<_>>()
            .join("")
    })
}

pub fn translate_to_element(el: &str, child: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    format!("<{el}{attr_txt}>{child}</{el}>")
}

pub fn translate_link(text: &str, url: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    format!("<a href=\"{url}\"{attr_txt}>{text}</a>")
}

pub fn translate_image(text: &str, url: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    format!("<img alt=\"{text}\" src=\"{url}\"{attr_txt}/>",)
}

pub fn translate_list_elements(lines: Vec<MarkdownText>) -> String {
    lines
        .iter()
        .map(|line| format!("<li>{}</li>", translate_text(line.to_vec())))
        .collect::<Vec<String>>()
        .join("")
}

pub fn translate_header(size: usize, text: MarkdownText, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!("<h{}{attr_txt}>{}</h{}>", size, translate_text(text), size)
}

pub fn translate_unordered_list(lines: Vec<MarkdownText>, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!(
        "<ul{attr_txt}>{}</ul>",
        translate_list_elements(lines.to_vec())
    )
}

pub fn translate_ordered_list(lines: Vec<MarkdownText>, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!(
        "<ol{attr_txt}>{}</ol>",
        translate_list_elements(lines.to_vec())
    )
}

pub fn translate_codeblock(lang: &str, code: &str, attr: &MarkdownAttributes) -> String {
    let trimmed_lang = lang.trim();
    let attr_txt = translate_attributes(attr);
    if trimmed_lang.starts_with("=") {
        if trimmed_lang.starts_with("=html") {
            code.into()
        } else {
            // Hide all verbatim blocks which are not of type html. To allow blocks used in other
            // media types like pdf or latex
            "".into()
        }
    } else {
        format!(
            "<pre><code class=\"lang-{}\"{attr_txt}>{}</code></pre>",
            trimmed_lang, code
        )
    }
}

pub fn translate_line(text: MarkdownText, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    let line = translate_text(text);
    if line.len() > 0 {
        format!("<p{attr_txt}>{}</p>", line)
    } else {
        format!("{}", line)
    }
}

pub fn translate_text(text: MarkdownText) -> String {
    text.iter()
        .map(|part| match part {
            MarkdownInline::Bold(text, attr) => {
                translate_to_element("strong", &translate_text(text.to_vec()), attr)
            }
            MarkdownInline::Italic(text, attr) => {
                translate_to_element("em", &translate_text(text.to_vec()), attr)
            }
            MarkdownInline::InlineCode(code, attr) => {
                translate_to_element("code", &translate_text(code.to_vec()), attr)
            }
            MarkdownInline::Link(text, url, attr) => translate_link(text, url, attr),
            MarkdownInline::Image(text, url, attr) => translate_image(text, url, attr),
            MarkdownInline::Plaintext(text, attr) => text.to_string(),
            MarkdownInline::LineBreak => "\n".into(),
            MarkdownInline::Span(text, attr) => {
                translate_to_element("span", &translate_text(text.to_vec()), attr)
            }
        })
        .collect::<Vec<String>>()
        .join("")
}


// `translate_text_raw` returns only the text content of a markdown text, ignoring any non text or
// stylistic components.
pub fn translate_text_raw(text: MarkdownText) -> String {
    text.iter()
        .map(|part| match part {
            MarkdownInline::Bold(text, _attr) => {
                translate_text_raw(text.to_vec())
            }
            MarkdownInline::Italic(text, _attr) => {
                translate_text(text.to_vec())
            }
            MarkdownInline::InlineCode(code, _attr) => {
                translate_text(code.to_vec())
            }
            MarkdownInline::Link(text, _url, _attr) => text.to_string(),
            MarkdownInline::Image(text, _url, _attr) => text.to_string(),
            MarkdownInline::Plaintext(text, _attr) => text.to_string(),
            MarkdownInline::LineBreak => "\n".into(),
            MarkdownInline::Span(text, _attr) => {
                translate_text(text.to_vec())
            }
        })
        .collect::<Vec<String>>()
        .join("")
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_markdown;

    use super::*;

    #[test]
    fn test_translate_boldtext() {
        assert_eq!(
            translate_text(vec![MarkdownInline::Bold(
                vec![MarkdownInline::Plaintext("bold af", None)],
                None
            )]),
            String::from("<strong>bold af</strong>")
        );
    }

    #[test]
    fn test_translate_italic() {
        assert_eq!(
            translate_text(vec![MarkdownInline::Italic(
                vec![MarkdownInline::Plaintext("italic af", None)],
                None
            )]),
            String::from("<em>italic af</em>")
        );
    }

    #[test]
    fn test_translate_inline_code() {
        assert_eq!(
            translate_text(vec![MarkdownInline::InlineCode(
                vec![MarkdownInline::Plaintext("code af", None)],
                None
            )]),
            String::from("<code>code af</code>")
        );
    }

    // Mostly examples from the djot spec
    #[test]
    fn test_e2e_precedence() {
        let (_, md) = parse_markdown("_This is *regular_ not strong* emphasis").unwrap();
        assert_eq!(
            translate(md),
            "<p><em>This is *regular</em> not strong* emphasis</p>"
        );

        let (_, md) = parse_markdown("*This is _strong* not regular_ emphasis").unwrap();
        assert_eq!(
            translate(md),
            "<p><strong>This is _strong</strong> not regular_ emphasis</p>"
        );

        let (_, md) = parse_markdown("[Link *](url)*").unwrap();
        assert_eq!(translate(md), "<p><a href=\"url\">Link *</a>*</p>");

        let (_, md) = parse_markdown("*Emphasis [*](url)").unwrap();
        assert_eq!(translate(md), "<p><strong>Emphasis [</strong>](url)</p>");

        // We don't yet support nesting inline elements
        // let (_, md) = parse_markdown(r#"_This is *strong within* regular emphasis_"#).unwrap();
        // assert_eq!(translate(md), "<p><em>This is <strong>strong within</strong> regular emphasis</em></p>");

        // Seems like _} is treated as a special case, and _ doesn't trigger an emphasis
        // let (_, md) = parse_markdown("{_Emphasized_}\n_}not emphasized{_").unwrap();
        // assert_eq!(translate(md), "<p><em>Emphasized</em>\n_}not emphasized{_</p>");
        //

        let (_, md) = parse_markdown("[My link text](http://example.com)").unwrap();
        assert_eq!(
            translate(md),
            "<p><a href=\"http://example.com\">My link text</a></p>"
        );

        // It should be possible to split the link accross multiple lines and the newline should be
        // ignored
        // let (_, md) = parse_markdown("[My link text](http://example.com?\nproduct_number=234234234234\n234234234234)").unwrap();
        // assert_eq!(translate(md), "<p><a href=\"http://example.com?product_number=234234234234234234234234\">My link text</a></p>\n");

        let (_, md) = parse_markdown(r#"![picture of a cat](cat.jpg)"#).unwrap();
        assert_eq!(
            translate(md),
            "<p><img alt=\"picture of a cat\" src=\"cat.jpg\"/></p>"
        );

        let (_, md) = parse_markdown("_emphasized text_").unwrap();
        assert_eq!(translate(md), "<p><em>emphasized text</em></p>");

        let (_, md) = parse_markdown("_emphasized text_\n\n*strong emphasis*").unwrap();
        assert_eq!(
            translate(md),
            "<p><em>emphasized text</em></p><p><strong>strong emphasis</strong></p>"
        );

        let (_, md) = parse_markdown("{#idName .class style=\"background-color:red\"}\n_emphasized text_\n\n{#id2 .cName width=\"100%\"}\n*strong emphasis*").unwrap();
        assert_eq!(translate(md), "\
            <p class=\"class\" id=\"idName\" style=\"background-color:red\"><em>emphasized text</em></p>\
            <p class=\"cName\" id=\"id2\" width=\"100%\"><strong>strong emphasis</strong></p>");
    }

    #[test]
    fn test_e2e_codeblock() {
        let md_val = "\
``` js
console.log()
```";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(
            translate(md),
            "<pre><code class=\"lang-js\">console.log()\n</code></pre>"
        );
    }

    #[test]
    fn test_e2e_verbatim_codeblock() {
        let md_val = "\
``` =html
<div id=\"idname\">bla</div>
```";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(translate(md), "<div id=\"idname\">bla</div>\n");
    }

    #[test]
    fn test_e2e_div() {
        let md_val = "\
{.className #idName}
:::
# Hello world

Message 
:::";
        let (left, md) = parse_markdown(md_val).unwrap();
        println!("LEFT {left:?}");
        assert_eq!(
            translate(md),
            "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1><p>Message </p></div>"
        );

        let md_val = "\
{.className #idName}
:::
# Hello world

Message 
:::
{.className #idName}
:::
# Hello world 2

Message 
:::
            ";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(
            translate(md),
            "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1><p>Message </p></div>\
                <div class=\"className\" id=\"idName\"><h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div>"
        );
    }

    #[test]
    fn test_e2e_div_nested() {
        let md_val = "\
{.className #idName}
:::::
# Hello world

{.className #idName}
:::
# Hello world 2

Message 
:::
Message 
:::::";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(
            translate(md),
            "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1><div class=\"className\" id=\"idName\">\
                <h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>");
    }

    #[test]
    fn test_e2e_div_nested_inverted() {
        let md_val = "\
{.className #idName}
:::
# Hello world

{.className #idName}
:::::
# Hello world 2

Message 
:::::

{.className #idName}
:::::
# Hello world 2

Message 
:::::

Message 
:::";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(
            translate(md),
            "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1><div class=\"className\" id=\"idName\">\
                <h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>");
    }

    #[test]
    fn test_e2e_div_nested_inverted_with_code_block() {
        let md_val = "\
{.className #idName}
:::
```html
bla
```
{.className #idName}
:::::
# Hello world 2

Message 
:::::

Message 
:::";
        let (_, md) = parse_markdown(md_val).unwrap();
        assert_eq!(
            translate(md),
            "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1><div class=\"className\" id=\"idName\">\
                <h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>");
    }



    #[test]
    fn test_e2e_div_nested_3() {
        let md_val = "\
{.c1 #i1}
:::
Message
:::
";
        let (_, md) = parse_markdown(md_val).unwrap();
        println!("{:?}", md);
        assert_eq!(
            translate(md),
            "<div class=\"c1\" id=\"i1\"><h1>Hello world</h1><div class=\"c2\" id=\"i2\">\
                <h1>Hello world</h1><p>Message </p></div><p>Message </p></div>"
        );
    }

    // #[test]
    // fn test_translate_link() {
    //     assert_eq!(
    //         translate_link(
    //             String::from("click me!"),
    //             String::from("https://github.com")
    //         ),
    //         String::from("<a href=\"https://github.com\">click me!</a>")
    //     );
    // }

    //     #[test]
    //     fn test_translate_image() {
    //         assert_eq!(
    //             translate_image(String::from("alt text"), String::from("https://github.com")),
    //             String::from("<img src=\"https://github.com\" alt=\"alt text\" />")
    //         );
    //     }

    //     #[test]
    //     fn test_translate_text() {
    //         let x = translate_text(vec![
    //             MarkdownInline::Plaintext(String::from(
    //                 "Foobar is a Python library for dealing with word pluralization.",
    //             )),
    //             MarkdownInline::Bold(String::from("bold")),
    //             MarkdownInline::Italic(String::from("italic"), None),
    //             MarkdownInline::InlineCode(String::from("code")),
    //             MarkdownInline::Link(String::from("tag"), String::from("https://link.com")),
    //             MarkdownInline::Image(String::from("tag"), String::from("https://link.com")),
    //             MarkdownInline::Plaintext(String::from(". the end!")),
    //         ]);
    //         assert_eq!(x, String::from("Foobar is a Python library for dealing with word pluralization.<b>bold</b><em>italic</em><code>code</code><a href=\"https://link.com\">tag</a><img src=\"https://link.com\" alt=\"tag\" />. the end!"));
    //         let x = translate_text(vec![]);
    //         assert_eq!(x, String::from(""));
    //     }

    //     #[test]
    //     fn test_translate_header() {
    //         assert_eq!(
    //             translate_header(1, vec![MarkdownInline::Plaintext(String::from("Foobar"))]),
    //             String::from("<h1>Foobar</h1>")
    //         );
    //     }

    //     #[test]
    //     fn test_translate_list_elements() {
    //         assert_eq!(
    //             translate_list_elements(vec![
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //             ]),
    //             String::from("<li>Foobar</li><li>Foobar</li><li>Foobar</li><li>Foobar</li>")
    //         );
    //     }

    //     #[test]
    //     fn test_translate_unordered_list() {
    //         assert_eq!(
    //             translate_unordered_list(vec![
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //             ]),
    //             String::from("<ul><li>Foobar</li><li>Foobar</li><li>Foobar</li><li>Foobar</li></ul>")
    //         );
    //     }

    //     #[test]
    //     fn test_translate_ordered_list() {
    //         assert_eq!(
    //             translate_ordered_list(vec![
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //                 vec![MarkdownInline::Plaintext(String::from("Foobar"))],
    //             ]),
    //             String::from("<ol><li>Foobar</li><li>Foobar</li><li>Foobar</li><li>Foobar</li></ol>")
    //         );
    //     }

    //     #[test]
    //     fn test_translate_codeblock() {
    //         assert_eq!(
    //             translate_codeblock(
    //                 String::from("python"),
    //                 String::from(
    //                     r#"
    // import foobar

    // foobar.pluralize(\'word\') # returns \'words\'
    // foobar.pluralize(\'goose\') # returns \'geese\'
    // foobar.singularize(\'phenomena\') # returns \'phenomenon\'
    // "#
    //                 )
    //             ),
    //             String::from(
    //                 r#"<pre><code class="lang-python">
    // import foobar

    // foobar.pluralize(\'word\') # returns \'words\'
    // foobar.pluralize(\'goose\') # returns \'geese\'
    // foobar.singularize(\'phenomena\') # returns \'phenomenon\'
    // </code></pre>"#
    //             )
    //         );
    //     }

    //     #[test]
    //     fn test_translate_line() {
    //         assert_eq!(
    //             translate_line(vec![
    //                 MarkdownInline::Plaintext(String::from("Foobar")),
    //                 MarkdownInline::Bold(String::from("Foobar")),
    //                 MarkdownInline::Italic(String::from("Foobar"), None),
    //                 MarkdownInline::InlineCode(String::from("Foobar")),
    //             ]),
    //             String::from("<p>Foobar<b>Foobar</b><em>Foobar</em><code>Foobar</code></p>")
    //         );
    //     }
}
