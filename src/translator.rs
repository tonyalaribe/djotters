use crate::{Markdown, MarkdownAttributes};
use crate::MarkdownInline;
use crate::MarkdownText;

pub fn translate(md: Vec<Markdown>) -> String {
    println!("{:?}", md);

    md.iter()
        .map(|bit| match bit {
            Markdown::Heading(size, line, attr) => translate_header(*size, line.to_vec(), attr),
            Markdown::UnorderedList(lines, attr) => translate_unordered_list(lines.to_vec(), attr),
            Markdown::OrderedList(lines, attr) => translate_ordered_list(lines.to_vec(), attr),
            Markdown::Codeblock(lang, code, attr) => {
                translate_codeblock(lang, code, attr)
            }
            Markdown::Line(line, attr) => translate_line(line.to_vec(), attr),
        })
        .collect::<Vec<String>>()
        .join("")
}

fn translate_attributes(opt: &MarkdownAttributes) -> String {
    opt.as_ref().map_or(String::new(), |hash_map|
        hash_map.iter()
            .map(|(k, v)| format!(" {}=\"{}\"", k , v))
            .collect::<Vec<_>>()
            .join("")
    )
}

fn translate_to_element(el: &str, child: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    let child_escaped = html_escape::encode_text(child);
    format!("<{el}{attr_txt}>{child_escaped}</{el}>")
}


fn translate_link(text: &str, url: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    format!("<a href=\"{url}\"{attr_txt}>{text}</a>")
}

fn translate_image(text: &str, url: &str, attrs: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attrs);
    format!("<img src=\"{url}\" alt=\"{text}\" {attr_txt}/>", )
}

fn translate_list_elements(lines: Vec<MarkdownText>) -> String {
    lines
        .iter()
        .map(|line| format!("<li>{}</li>", translate_text(line.to_vec())))
        .collect::<Vec<String>>()
        .join("")
}

fn translate_header(size: usize, text: MarkdownText, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!("<h{}{attr_txt}>{}</h{}>", size, translate_text(text), size)
}

fn translate_unordered_list(lines: Vec<MarkdownText>, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!("<ul{attr_txt}>{}</ul>", translate_list_elements(lines.to_vec()))
}

fn translate_ordered_list(lines: Vec<MarkdownText>, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!("<ol{attr_txt}>{}</ol>", translate_list_elements(lines.to_vec()))
}

fn translate_codeblock(lang: &str, code: &str, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    format!("<pre><code class=\"lang-{}\"{attr_txt}>{}</code></pre>", lang, code)
}

fn translate_line(text: MarkdownText, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    let line = translate_text(text);
    if line.len() > 0 {
        format!("<p{attr_txt}>{}</p>", line)
    } else {
        format!("{}", line)
    }
}

fn translate_text(text: MarkdownText) -> String {
    text.iter()
        .map(|part| match part {
            MarkdownInline::Bold(text, attr) => translate_to_element("strong", text, attr),
            MarkdownInline::Italic(text, attr) => translate_to_element("em" ,text, attr),
            MarkdownInline::InlineCode(code, attr) => translate_to_element("code", code, attr),
            MarkdownInline::Link(text, url, attr) => translate_link(text, url, attr),
            MarkdownInline::Image(text, url, attr) => translate_image(text, url, attr),
            MarkdownInline::Plaintext(text, attr) => html_escape::encode_text(text).to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_translate_boldtext() {
//         assert_eq!(
//             translate_boldtext(String::from("bold af")),
//             String::from("<b>bold af</b>")
//         );
//     }

//     #[test]
//     fn test_translate_italic() {
//         assert_eq!(
//             translate_italic(String::from("italic af")),
//             String::from("<em>italic af</em>")
//         );
//     }

//     #[test]
//     fn test_translate_inline_code() {
//         assert_eq!(
//             translate_inline_code(String::from("code af")),
//             String::from("<code>code af</code>")
//         );
//     }

//     #[test]
//     fn test_translate_link() {
//         assert_eq!(
//             translate_link(
//                 String::from("click me!"),
//                 String::from("https://github.com")
//             ),
//             String::from("<a href=\"https://github.com\">click me!</a>")
//         );
//     }

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
// }
