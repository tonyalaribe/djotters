use std::collections::HashMap;

pub mod parser;
pub mod translator;
pub mod tests;

pub type MarkdownText<'a> = Vec<MarkdownInline<'a>>;

// To hold parsed markdown attrributes. eg:
// ```
//  { #idValue .className1 .className2 class="extraclass3" data-extra-attribute="attribute value" }
// ```
pub type MarkdownAttributes<'a> = Option<HashMap<&'a str, String>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Markdown<'a> {
    Heading(usize, MarkdownText<'a>, MarkdownAttributes<'a>),
    OrderedList(Vec<MarkdownText<'a>>, MarkdownAttributes<'a>),
    UnorderedList(Vec<MarkdownText<'a>>, MarkdownAttributes<'a>),
    Line(MarkdownText<'a>, MarkdownAttributes<'a>),
    Codeblock(&'a str, &'a str, MarkdownAttributes<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MarkdownInline<'a> {
    Link(&'a str, &'a str, MarkdownAttributes<'a>),
    Image(&'a str, &'a str, MarkdownAttributes<'a>),
    InlineCode(&'a str, MarkdownAttributes<'a>),
    Bold(&'a str, MarkdownAttributes<'a>),
    Italic(&'a str, MarkdownAttributes<'a>),
    Plaintext(&'a str, MarkdownAttributes<'a>),
}

pub fn markdown(md: &str) -> String {
    match parser::parse_markdown(md) {
        Ok((_, m)) => translator::translate(m),
        Err(_) => String::from("Sorry, this did not seem to work! Maybe your markdown was not well formed, have you hit [Enter] after your last line?"),
    }
}
