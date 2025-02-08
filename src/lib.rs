use std::collections::HashMap;

pub mod parser;
pub mod tests;
pub mod translator;

pub use parser::*;
pub use translator::*;

pub type MarkdownText = Vec<MarkdownInline>;

// To hold parsed markdown attrributes. eg:
// ```
//  { #idValue .className1 .className2 class="extraclass3" data-extra-attribute="attribute value" }
// ```
pub type MarkdownAttributes = Option<HashMap<String, String>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Markdown {
    Heading(usize, MarkdownText, MarkdownAttributes),
    OrderedList(Vec<MarkdownText>, MarkdownAttributes),
    UnorderedList(Vec<MarkdownText>, MarkdownAttributes),
    BlockQuotes(Vec<Markdown>, MarkdownAttributes),
    Line(MarkdownText, MarkdownAttributes),
    Codeblock(String, String, MarkdownAttributes),
    Div(Option<String>, Vec<Markdown>, MarkdownAttributes),
    LineBreak,
    Table(
        Vec<MarkdownText>,
        Vec<Alignment>,
        Vec<Vec<MarkdownText>>,
    ),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MarkdownInline {
    Link(String, String, MarkdownAttributes),
    Image(String, String, MarkdownAttributes),
    InlineCode(MarkdownText, MarkdownAttributes),
    Bold(MarkdownText, MarkdownAttributes),
    Highlight(MarkdownText, MarkdownAttributes),
    Subscript(MarkdownText, MarkdownAttributes),
    Superscript(MarkdownText, MarkdownAttributes),
    Insert(MarkdownText, MarkdownAttributes),
    Delete(MarkdownText, MarkdownAttributes),
    Italic(MarkdownText, MarkdownAttributes),
    Span(MarkdownText, MarkdownAttributes),
    Plaintext(String, MarkdownAttributes),
    LineBreak,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

pub fn markdown(md: &str) -> String {
    match parser::parse_markdown(md) {
        Ok((_, m)) => translator::translate(m),
        Err(_) => String::from("Sorry, this did not seem to work! Maybe your markdown was not well formed, have you hit [Enter] after your last line?"),
    }
}
