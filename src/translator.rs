use crate::MarkdownText;
use crate::{Alignment, MarkdownInline};
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
            Markdown::BlockQuotes(lines, attr) => translate_blockquote(lines, attr),
            Markdown::Line(line, attr) => translate_line(line.to_vec(), attr),
            Markdown::Div(title, content, attr) => translate_div(title, content, attr),
            Markdown::LineBreak => "\n".into(),
            Markdown::Table(headings, alignments, rows) => {
                translate_table(headings.to_vec(), alignments.to_vec(), rows.to_vec())
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn translate_alignment(alignment: &Alignment) -> &str {
    match alignment {
        Alignment::Left => "left",
        Alignment::Center => "center",
        Alignment::Right => "right",
    }
}

pub fn translate_table(
    headers: Vec<MarkdownText>,
    alignments: Vec<Alignment>,
    rows: Vec<Vec<MarkdownText>>,
) -> String {
    let thead = headers
        .iter()
        .map(|header_cell| format!("<th>{}</th>", translate_text(header_cell.to_vec())))
        .collect::<String>();

    let tbody = rows
        .iter()
        .map(|row| {
            let tr = row
                .iter()
                .enumerate()
                .map(|(i, cell)| {
                    let align = alignments
                        .get(i)
                        .map_or("", |alignment| translate_alignment(alignment));
                    format!(
                        "<td style=\"text-align:{};\">{}</td>",
                        align,
                        translate_text(cell.to_vec())
                    )
                })
                .collect::<String>();
            format!("<tr>{}</tr>", tr)
        })
        .collect::<String>();

    format!(
        "<table><thead><tr>{}</tr></thead><tbody>{}</tbody></table>",
        thead, tbody
    )
}

pub fn translate_div(
    title: &Option<String>,
    content: &Vec<Markdown>,
    attr1: &MarkdownAttributes,
) -> String {
    let mut attr = attr1.clone();
    if let Some(t) = title {
        if let Some(attributes) = &mut attr {
            if let Some(val) = attributes.get_mut("class") {
                *val = format!("{} {}", val, t);
            } else {
                attributes.insert("class".into(), t.to_string());
            }
        } else {
            let mut new_attr = HashMap::new();
            new_attr.insert("class".into(), t.to_string());
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

pub fn translate_blockquote(content: &Vec<Markdown>, attr: &MarkdownAttributes) -> String {
    let attr_txt = translate_attributes(attr);
    let inner = translate(content.clone());
    format!("<blockquote{attr_txt}>\n{}\n</blockquote>", inner)
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
            MarkdownInline::Plaintext(text, _attr) => html_escape::encode_text(text).to_string(),
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
            MarkdownInline::Bold(text, _attr) => translate_text_raw(text.to_vec()),
            MarkdownInline::Italic(text, _attr) => translate_text(text.to_vec()),
            MarkdownInline::InlineCode(code, _attr) => translate_text(code.to_vec()),
            MarkdownInline::Link(text, _url, _attr) => text.to_string(),
            MarkdownInline::Image(text, _url, _attr) => text.to_string(),
            MarkdownInline::Plaintext(text, _attr) => text.to_string(),
            MarkdownInline::LineBreak => "\n".into(),
            MarkdownInline::Span(text, _attr) => translate_text(text.to_vec()),
        })
        .collect::<Vec<String>>()
        .join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_markdown;
    use pretty_assertions::assert_eq; // Better diff output
    use test_case::test_case;

    // -------------------------------------------------------------------
    // Inline element translation tests
    // -------------------------------------------------------------------
    #[test_case(vec![MarkdownInline::Bold(vec![MarkdownInline::Plaintext("bold af".into(), None)],None)],  "<strong>bold af</strong>" ; "Bold")]
    #[test_case(vec![MarkdownInline::Italic(vec![MarkdownInline::Plaintext("italic af".into(), None)],None)], "<em>italic af</em>" ; "Italic")]
    #[test_case(vec![MarkdownInline::InlineCode(vec![MarkdownInline::Plaintext("code af".into(), None)],None)], "<code>code af</code>" ; "InlineCode")]
    fn test_translate_inline_elements(input: Vec<MarkdownInline>, output: &str) {
        assert_eq!(translate_text(input), output)
    }

    // -------------------------------------------------------------------
    // End-to-end tests for inline precedence, links, images, and emphasis.
    // -------------------------------------------------------------------
    #[test_case("_This is *regular_ not strong* emphasis","<p><em>This is *regular</em> not strong* emphasis</p>" ; "Precedence1")]
    #[test_case("*This is _strong* not regular_ emphasis","<p><strong>This is _strong</strong> not regular_ emphasis</p>" ; "Precedence2")]
    #[test_case("[Link *](url)*","<p><a href=\"url\">Link *</a>*</p>" ; "LinkWithAsterisk")]
    #[test_case("*Emphasis [*](url)", "<p><strong>Emphasis [</strong>](url)</p>" ; "EmphasisWithLink")]
    #[test_case("[My link text](http://example.com)","<p><a href=\"http://example.com\">My link text</a></p>" ; "BasicLink")]
    #[test_case("![picture of a cat](cat.jpg)","<p><img alt=\"picture of a cat\" src=\"cat.jpg\"/></p>" ; "Image")]
    #[test_case("_emphasized text_","<p><em>emphasized text</em></p>" ; "EmphasizedText")]
    #[test_case("_emphasized text_\n\n*strong emphasis*","<p><em>emphasized text</em></p>\n<p><strong>strong emphasis</strong></p>" ; "EmphasisStrong")]
    #[test_case(
        "{#idName .class style=\"background-color:red\"}\n_emphasized text_\n\n{#id2 .cName width=\"100%\"}\n*strong emphasis*",
        "<p class=\"class\" id=\"idName\" style=\"background-color:red\"><em>emphasized text</em></p>\n<p class=\"cName\" id=\"id2\" width=\"100%\"><strong>strong emphasis</strong></p>" ; 
        "Attributes"
    )]
    fn test_e2e_precedence_and_links(input: &str, output: &str) {
        let (_, md) = parse_markdown(input).expect("failed to parse markdown");
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // End-to-end code block tests.
    // -------------------------------------------------------------------
    #[test_case("``` js\nconsole.log()\n```", "<pre><code class=\"lang-js\">console.log()\n</code></pre>" ; "JSCodeBlock")]
    #[test_case("``` =html\n<div id=\"idname\">bla</div>\n```", "<div id=\"idname\">bla</div>\n" ; "VerbatimHTMLCodeBlock")]
    fn test_e2e_codeblocks(input: &str, output: &str) {
        let (_, md) = parse_markdown(input).expect("failed to parse markdown");
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // End-to-end div block tests (including nested cases).
    // -------------------------------------------------------------------
    #[test_case(
        "{.className #idName}\n:::\n# Hello world\n\nMessage \n:::",
        "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1>\n<p>Message </p></div>" ; "SingleDiv"
    )]
    #[test_case(
        "{.className #idName}\n:::\n# Hello world\n\nMessage \n:::\n{.className #idName}\n:::\n# Hello world 2\n\nMessage \n:::",
        "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1>\n<p>Message </p></div><div class=\"className\" id=\"idName\"><h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div>" ; "TwoDivs"
    )]
    #[test_case(
        "{.className #idName}\n:::::\n# Hello world\n\n{.className #idName}\n:::\n# Hello world 2\n\nMessage \n:::\nMessage \n:::::",
        "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1>\n<div class=\"className\" id=\"idName\"><h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>" ; "NestedDiv"
    )]
    #[test_case(
        "{.className #idName}\n:::\n# Hello world\n\n{.className #idName}\n:::::\n# Hello world 2\n\nMessage \n:::::\n\n{.className #idName}\n:::::\n# Hello world 2\n\nMessage \n:::::\n\nMessage \n:::",
        "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1>\n<div class=\"className\" id=\"idName\"><h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>" ; "MultipleNestedDivs"
    )]
    #[test_case(
        "{.className #idName}\n:::\n```html\nbla\n```\n{.className #idName}\n:::::\n# Hello world 2\n\nMessage \n:::::\n\nMessage \n:::",
        "<div class=\"className\" id=\"idName\"><h1 id=\"hello-world\">Hello world</h1>\n<div class=\"className\" id=\"idName\"><h1 id=\"hello-world-2\">Hello world 2</h1><p>Message </p></div><p>Message </p></div>" ; "DivWithCodeBlock"
    )]
    #[test_case(
        "{.c1 #i1}\n:::\nMessage\n:::",
        "<div class=\"c1\" id=\"i1\"><h1>Hello world</h1><div class=\"c2\" id=\"i2\"><h1>Hello world</h1>\n<p>Message </p>\n</div>\n<p>Message </p></div>" ; "DivWithNestedHeading"
    )]
    fn test_e2e_div_blocks(input: &str, output: &str) {
        let (_, md) = parse_markdown(input).expect("failed to parse markdown");
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // End-to-end inline link tests.
    // -------------------------------------------------------------------
    #[test_case(
        "Message another [link Alt](/link) \nmore text **title**",
        "<p>Message another <a href=\"/link\">link Alt</a> \nmore text <strong>title</strong></p>" ; "InlineLinkWithNewline"
    )]
    #[test_case(
        "Message another [link Alt](/link) more text **title**",
        "<p>Message another <a href=\"/link\">link Alt</a></p>" ; "InlineLinkWithoutNewline"
    )]
    fn test_e2e_inline_links(input: &str, output: &str) {
        let (_, md) = parse_markdown(input).expect("failed to parse markdown");
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // End-to-end ordered list tests.
    // -------------------------------------------------------------------
    #[test_case(
        "1. Link 1\n2. Link 2 \n\n3. Link 3 \n\n\n4. Link 4",
        "<ol><li>Link 1</li><li>Link 2 </li><li>Link 3 </li><li>Link 4</li></ol>" ; "OrderedListSimple"
    )]
    #[test_case(
        "1. Link 1\n2. Link 2 \n\n    a. link a\n    b. link b \n\n3. Link 3 \n4. Link 4",
        "<ol><li>Link 1</li><li>Link 2 </li><li>Link 3 </li><li>Link 4</li></ol>" ; "OrderedListNested"
    )]
    fn test_e2e_ordered_lists(input: &str, output: &str) {
        let (_, md) = parse_markdown(input).expect("failed to parse markdown");
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // End-to-end table tests.
    // -------------------------------------------------------------------
    #[test]
    fn test_e2e_tables() {
        let input = "\
| Material | Quantity | Catch-phrase  |
| -------- | -------: | :-----------: |
| cotton   |       42 |   Practical!  |
| wool     |       17 |     Warm!     |
| silk     |        4 |    Smooth!    |";
        let expected = "<table><thead><tr><th>Material</th><th>Quantity</th><th>Catch-phrase</th></tr></thead><tbody><tr>\
<td style=\"text-align:left;\">cotton</td><td style=\"text-align:right;\">42</td>\
<td style=\"text-align:center;\">Practical!</td></tr><tr>\
<td style=\"text-align:left;\">wool</td><td style=\"text-align:right;\">17</td>\
<td style=\"text-align:center;\">Warm!</td></tr><tr>\
<td style=\"text-align:left;\">silk</td><td style=\"text-align:right;\">4</td>\
<td style=\"text-align:center;\">Smooth!</td></tr></tbody></table>";
        let (_, md) = parse_markdown(input.trim()).expect("failed to parse markdown");
        assert_eq!(translate(md), expected);
    }

    // -------------------------------------------------------------------
    // Blockquote tests.
    // -------------------------------------------------------------------
    #[test_case("> Basic\n> block _quote_.","<blockquote>\n<p>Basic\nblock <em>quote</em>.</p>\n</blockquote>" ; "BasicBlockquote")]
    #[test_case("> Lazy\nblock _quote_.","<blockquote>\n<p>Lazy\nblock <em>quote</em>.</p>\n</blockquote>" ; "LazyBlockquote")]
    #[test_case("> block\n>\n> quote","<blockquote>\n<p>block</p>\n<p>quote</p>\n</blockquote>" ; "BlockquoteMultipleParagraphs")]
    #[test_case("> block\n\n> quote","<blockquote>\n<p>block</p>\n</blockquote>\n<blockquote>\n<p>quote</p>\n</blockquote>" ; "SeparateBlockquotes")]
    #[test_case(
        "> > > nested",
        "<blockquote>\n<blockquote>\n<blockquote>\n<p>nested</p>\n</blockquote>\n</blockquote>\n</blockquote>" ; "NestedBlockquote"
    )]
    #[test_case(
        "> > > nested\nlazy",
        "<blockquote>\n<blockquote>\n<blockquote>\n<p>nested\nlazy</p>\n</blockquote>\n</blockquote>\n</blockquote>" ; "NestedBlockquoteLazyContinuation"
    )]
    #[test_case(
        "> > > nested\n> lazy",
        "<blockquote>\n<blockquote>\n<blockquote>\n<p>nested\nlazy</p>\n</blockquote>\n</blockquote>\n</blockquote>" ; "NestedBlockquoteWithLazyMarker"
    )]
    #[test_case("> nested\n>\n> > more","<blockquote>\n<p>nested</p>\n<blockquote>\n<p>more</p>\n</blockquote>\n</blockquote>" ; "BlockquoteWithInnerBlockquote")]
    #[test_case(">not blockquote","<p>&gt;not blockquote</p>" ; "NotBlockquoteNoSpace")]
    #[test_case(">> not blockquote","<p>&gt;&gt; not blockquote</p>" ; "NotBlockquoteDoubleGreater")]
    #[test_case(">","<blockquote>\n\n</blockquote>" ; "EmptyBlockquote")]
    #[test_case("> # Heading","<blockquote>\n<h1 id=\"heading\">Heading</h1>\n</blockquote>" ; "BlockquoteWithHeading")]
    #[test_case("> hi\n>there","<blockquote>\n<p>hi\n&gt;there</p>\n</blockquote>" ; "BlockquoteWithMissingMarkerOnSecondLine")]
    #[test_case("aaa\n> bbb","<p>aaa\n&gt; bbb</p>" ; "ParagraphFollowedByBlockquoteOnSameLine")]
    #[test_case("aaa\n\n> bbb","<p>aaa</p>\n<blockquote>\n<p>bbb</p>\n</blockquote>" ; "ParagraphThenBlockquoteSeparatedByBlankLine")]
    fn test_blockquotes(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output)
    }

    // -------------------------------------------------------------------
    // HTML output tests for inline and block attributes.
    // -------------------------------------------------------------------

    #[test_case("foo привет{.ru}",r#"<p>foo <span class="ru">привет</span></p>"#; "InlineAttributeNonAscii")]
    #[test_case("(some text){.attr}",r#"<p>(some <span class="attr">text)</span></p>"#; "InlineAttributeWithParentheses")]
    #[test_case("[some text]{.attr}",r#"<p><span class="attr">some text</span></p>"#; "InlineAttributeSquareBrackets")]
    #[test_case("a *b{#id key=\"*\"}*",r#"<p>a <strong><span id="id" key="*">b</span></strong></p>"#; "EmphasisBeforeAttributeWithPotentialCloser")]
    #[test_case("a *b{#id key=\"*\"}o",r#"<p>a <span id="id" key="*">*b</span>o</p>"#; "EmphasisMissingClosingAfterAttribute")]
    #[test_case("hi{key=\"{#hi\"}",r#"<p><span key="{#hi">hi</span></p>"#; "AttributeWithBracesInQuotes")]
    #[test_case("hi\\{key=\"abc{#hi}\"",r#"<p>hi{key=&ldquo;<span id="hi">abc</span>&rdquo;</p>"#; "EscapedBracePreventsAttributeParsing")]
    #[test_case("hi{key=\"\\\"#hi\"}",r#"<p><span key="&quot;#hi">hi</span></p>"#; "AttributeWithEscapedQuoteAtStart")]
    #[test_case("hi{key=\"hi\\\"#hi\"}",r#"<p><span key="hi&quot;#hi">hi</span></p>"#; "AttributeWithEscapedQuoteMid")]
    #[test_case("hi{#id .class\nkey=\"value\"}",r#"<p><span class="class" id="id" key="value">hi</span></p>"#; "InlineAttributeWithLineBreak")]
    #[test_case("{#id} at beginning",r#"<p> at beginning</p>"#; "AttributeWithoutTargetAtBeginning")]
    #[test_case("After {#id} space\n{.class}","<p>After  space\n</p>"; "AttributeNotAttachedAfterWord")]
    #[test_case("{#id .class}\nA paragraph",r#"<p class="class" id="id">A paragraph</p>"#; "BlockAttributeBeforeParagraph")]
    #[test_case("{#id .class\n  style=\"color:red\"}\nA paragraph",r#"<p class="class" id="id" style="color:red">A paragraph</p>"#; "BlockAttributeWithIndentedContinuation")]
    #[test_case("{#id .cla*ss*",r#"<p>{#id .cla<strong>ss</strong></p>"#; "InvalidBlockAttributeParsedAsText")]
    #[test_case("{#id}\n{key=val}\n{.foo .bar}\n{key=val2}\n{.baz}\n{#id2}\nOkay",r#"<p class="foo bar baz" id="id2" key="val2">Okay</p>"#; "ConsecutiveAttributeBlocksOverrideAndAccumulate")]
    #[test_case("{#id}\n> Block quote",
r#"<blockquote id="id">
<p>Block quote</p>
</blockquote>"#
        ; "BlockAttributeOnBlockquote")]
    #[test_case("{#id}\n# Heading",
r#"<section id="id">
<h1>Heading</h1>
</section>"#
        ; "BlockAttributeOnHeading")]
    #[test_case("{.blue}\n- - - - -",
r#"<hr class="blue">"#
        ; "BlockAttributeOnHorizontalRule")]
    #[test_case("{highlight=3}\n``` ruby\nx = 3\n```",
r#"<pre highlight="3"><code class="language-ruby">x = 3
</code></pre>"#
        ; "BlockAttributeOnFencedCodeBlock")]
    #[test_case("{.special}\n1. one\n2. two",
r#"<ol class="special">
<li>
one
</li>
<li>
two
</li>
</ol>"#
        ; "BlockAttributeOnOrderedList")]
    #[test_case("> {.foo}\n> > {.bar}\n> > nested",
r#"<blockquote>
<blockquote class="foo">
<p class="bar">nested</p>
</blockquote>
</blockquote>"#
        ; "NestedBlockquoteWithAttributes")]
    #[test_case("foo{#ident % this is a comment % .class}",r#"<p><span class="class" id="ident">foo</span></p>"#; "InlineAttributeWithCommentDelimited")]
    #[test_case("foo{#ident % this is a comment}",r#"<p><span id="ident">foo</span></p>"#; "InlineAttributeWithTrailingComment")]
    #[test_case("{% This is  a comment before a\n  block-level item. %}\nParagraph.",r#"<p>Paragraph.</p>"#; "BlockLevelCommentBeforeParagraph")]
    #[test_case("hi{}",r#"<p>hi</p>"#; "EmptyInlineAttribute")]
    #[test_case("{}\nhi",r#"<p>hi</p>"#; "EmptyBlockAttribute")]
    #[test_case("text{a=x\n\nhello","<p>text{a=x</p>\n<p>hello</p>"; "NonAttributeBlockParagraphs")]
    #[test_case("{a=x\nhello","<p>{a=x\nhello</p>"; "NonAttributeBlockSingleParagraph")]
    #[test_case("text{a=x\n# non-heading",
r#"<p>text{a=x
# non-heading</p>"#
        ; "NonAttributeBlockWithHash")]
    #[test_case("{a=x\n# non-heading",
r#"<p>{a=x
# non-heading</p>"#
        ; "NonAttributeBlockStartingBraceWithHash")]
    fn test_attributes_html(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }

    #[test_case("`<a>`{=html}", "<p><a></p>" ; "RawInlineContent")]
    #[test_case("``` =html\n<table>\n```", "<table>" ; "RawBlockContent")]
    #[test_case("`<b>foo</b>`{=html #id}", "<p><code>&lt;b&gt;foo&lt;/b&gt;</code>{=html #id>\n<code></code></p>" ; "MixedRawAndRegularAttributes")]
    #[test_case("{.foo}\n``` =html\n<table>\n```", "<table>" ; "RawContentIgnoresAttributes")]
    fn test_raw_content(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }

    #[test_case(
        "This is a [test of\n*color*]{.blue}.", 
        "<p>This is a <span class=\"blue\">test of\n<strong>color</strong></span>.</p>" ; 
        "SpanWithLineBreakAndEmphasis"
    )]
    #[test_case(
        "not a [span] {#id}.", 
        "<p>not a [span] .</p>" ; 
        "NotASpanDueToSpaceBeforeAttribute"
    )]
    #[test_case(
        "[nested [span]{.blue}]{#ident}", 
        "<p><span id=\"ident\">nested <span class=\"blue\">span</span></span></p>" ; 
        "NestedSpan"
    )]
    fn test_spans(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }

    #[test_case("hi  \nthere  ", "<p>hi  \nthere</p>" ; "ParaWithHardLineBreaks")]
    fn test_para(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }

    #[test_case(
        "## Heading", 
        "<section id=\"Heading\">\n<h2>Heading</h2>\n</section>" ; 
        "Heading_H2"
    )]
    #[test_case(
        "# Heading\n\n# another", 
        "<section id=\"Heading\">\n<h1>Heading</h1>\n</section>\n<section id=\"another\">\n<h1>another</h1>\n</section>" ; 
        "Multiple_Headings"
    )]
    #[test_case(
        "# Heading\n# continued", 
        "<section id=\"Heading-continued\">\n<h1>Heading\ncontinued</h1>\n</section>" ; 
        "Heading_Continued"
    )]
    #[test_case(
        "##\nheading\n\npara", 
        "<section id=\"heading\">\n<h2>heading</h2>\n<p>para</p>\n</section>" ; 
        "Heading_With_Paragraph"
    )]
    #[test_case("##", "<section id=\"s-1\">\n<h2></h2>\n</section>" ; "Empty_Heading")]
    #[test_case(
        "## Heading\n### Next level", 
        "<section id=\"Heading\">\n<h2>Heading</h2>\n<section id=\"Next-level\">\n<h3>Next level</h3>\n</section>\n</section>" ; 
        "Nested_Headings"
    )]
    #[test_case(
        "# Heading\nlazy", 
        "<section id=\"Heading-lazy\">\n<h1>Heading\nlazy</h1>\n</section>" ; 
        "Heading_With_Lazy"
    )]
    #[test_case(
        "# Heading\nlazy\n# more\nlazy\n\ntext", 
        "<section id=\"Heading-lazy-more-lazy\">\n<h1>Heading\nlazy\nmore\nlazy</h1>\n<p>text</p>\n</section>" ; 
        "Heading_With_Multiple_Lazy_Lines"
    )]
    #[test_case(
        "##Notheading", 
        "<p>##Notheading</p>" ; 
        "Not_A_Heading"
    )]
    #[test_case("   ##    Heading", "<section id=\"Heading\">\n<h2>Heading</h2>\n</section>" ; "Heading_With_Leading_Spaces")]
    #[test_case(
        "## heading ##", 
        "<section id=\"heading\">\n<h2>heading ##</h2>\n</section>" ; 
        "Heading_With_Trailing_Hash"
    )]
    #[test_case(
        "# # heading", 
        "<section id=\"heading\">\n<h1># heading</h1>\n</section>" ; 
        "Heading_Starting_With_Hash"
    )]
    #[test_case(
        "{#Foo-bar}\nParagraph\n\n# Foo bar\n\n## Foo  bar\n\n{#baz}\n# Foo bar", 
        "<p id=\"Foo-bar\">Paragraph</p>\n<section id=\"Foo-bar-1\">\n<h1>Foo bar</h1>\n<section id=\"Foo-bar-2\">\n<h2>Foo  bar</h2>\n</section>\n</section>\n<section id=\"baz\">\n<h1>Foo bar</h1>\n</section>" ; 
        "Auto_Identifiers"
    )]
    #[test_case(
        "See [Introduction][]\n\n# Introduction", 
        "<p>See <a href=\"#Introduction\">Introduction</a>.</p>\n<section id=\"Introduction\">\n<h1>Introduction</h1>\n</section>" ; 
        "Implicit_Header_Reference"
    )]
    #[test_case(
        "See [Introduction][]\n\n{#foo}\n# Introduction", 
        "<p>See <a href=\"#foo\">Introduction</a>.</p>\n<section id=\"foo\">\n<h1>Introduction</h1>\n</section>" ; 
        "Implicit_Header_With_Explicit_Id"
    )]
    #[test_case(
        "See [Introduction][]\n\n# Introduction\n\n[Introduction]: #bar", 
        "<p>See <a href=\"#bar\">Introduction</a>.</p>\n<section id=\"Introduction\">\n<h1>Introduction</h1>\n</section>" ; 
        "Implicit_Header_With_Link_Definition"
    )]
    fn test_headings(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }

    #[test_case(
        "- one\n- two",
        "<ul>\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ul>" ; "List_Simple"
    )]
    #[test_case(
        "- one\n - two\n  - three",
        "<ul>\n<li>\none\n- two\n- three\n</li>\n</ul>" ; "List_IndentedContinuation"
    )]
    #[test_case(
        "- one\n\n - two\n\n  - three",
        "<ul>\n<li>\none\n<ul>\n<li>\ntwo\n<ul>\n<li>\nthree\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>" ; "List_Nested"
    )]
    #[test_case(
        "- one\n  and\n\n  another paragraph\n\n  - a list\n\n- two",
        "<ul>\n<li>\n<p>one\nand</p>\n<p>another paragraph</p>\n<ul>\n<li>\na list\n</li>\n</ul>\n</li>\n<li>\n<p>two</p>\n</li>\n</ul>" ; "List_WithParagraphsAndNested"
    )]
    #[test_case(
        "- one\nlazy\n- two",
        "<ul>\n<li>\none\nlazy\n</li>\n<li>\ntwo\n</li>\n</ul>" ; "List_LazyContinuation"
    )]
    #[test_case(
        "- a\n- b\n+ c",
        "<ul>\n<li>\na\n</li>\n<li>\nb\n</li>\n</ul>\n<ul>\n<li>\nc\n</li>\n</ul>" ; "List_MultipleMarkers"
    )]
    #[test_case(
        "- a\n\n- b",
        "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ul>" ; "List_ItemsWithBlankLines"
    )]
    #[test_case(
        "- a\n  - b\n\n  - c\n- d",
        "<ul>\n<li>\na\n- b\n<ul>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>" ; "List_SublistContinuation"
    )]
    #[test_case(
        "- a\n  - b\n\n  - c\n\n- d",
        "<ul>\n<li>\na\n- b\n<ul>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>" ; "List_SublistContinuationExtraBlank"
    )]
    #[test_case(
        "- a\n\n  b\n- c",
        "<ul>\n<li>\n<p>a</p>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>" ; "List_ItemMultipleParagraphs"
    )]
    #[test_case(
        "- a\n\n  - b\n  - c\n- d",
        "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>" ; "List_NestedItemsNoParagraphs"
    )]
    #[test_case(
        "- a\n\n  - b\n  - c\n\n- d",
        "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>" ; "List_NestedItemsExtraBlank"
    )]
    #[test_case(
        "- a\n\n  * b\ncd",
        "<ul>\n<li>\na\n<ul>\n<li>\nb\ncd\n</li>\n</ul>\n</li>\n</ul>" ; "List_MixedMarkerContinuation"
    )]
    #[test_case(
        "- - - a",
        "<ul>\n<li>\n<ul>\n<li>\n<ul>\n<li>\na\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>" ; "List_MultipleNestedMarkers"
    )]
    #[test_case(
        "1. one\n1. two",
        "<ol>\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_Simple"
    )]
    #[test_case(
        "1. one\n\n 1. two",
        "<ol>\n<li>\none\n<ol>\n<li>\ntwo\n</li>\n</ol>\n</li>\n</ol>" ; "Ordered_Nested"
    )]
    #[test_case(
        "4. one\n5. two",
        "<ol start=\"4\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_WithStart"
    )]
    #[test_case(
        "1) one\n2) two",
        "<ol>\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_Paren"
    )]
    #[test_case(
        "(1) one\n(2) two",
        "<ol>\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_Paren2"
    )]
    #[test_case(
        "(a) one\n(b) two",
        "<ol type=\"a\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_AlphaLowercase"
    )]
    #[test_case(
        "(D) one\n(E) two",
        "<ol start=\"4\" type=\"A\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_AlphaUppercase"
    )]
    #[test_case(
        "a. one\nb. two",
        "<ol type=\"a\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_AlphaLowercaseDot"
    )]
    #[test_case(
        "i. one\nii. two",
        "<ol type=\"i\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_RomanLowercase"
    )]
    #[test_case(
        "xli) one\nxlii) two",
        "<ol start=\"41\" type=\"i\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_RomanLowercaseWithStart"
    )]
    #[test_case(
        "(IV) one\n(V) two",
        "<ol start=\"4\" type=\"I\">\n<li>\none\n</li>\n<li>\ntwo\n</li>\n</ol>" ; "Ordered_RomanUppercase"
    )]
    #[test_case(
        "i. a\nii. b",
        "<ol type=\"i\">\n<li>\na\n</li>\n<li>\nb\n</li>\n</ol>" ; "Ordered_RomanLowercaseSimple"
    )]
    #[test_case(
        "i. a\nj. b",
        "<ol start=\"9\" type=\"a\">\n<li>\na\n</li>\n<li>\nb\n</li>\n</ol>" ; "Ordered_AlphaLowercaseStart9"
    )]
    #[test_case(
        "i. a\ni. b",
        "<ol type=\"i\">\n<li>\na\n</li>\n<li>\nb\n</li>\n</ol>" ; "Ordered_AmbiguousRoman"
    )]
    #[test_case(
        "I. a\nII. b\nE. d",
        "<ol type=\"I\">\n<li>\na\n</li>\n<li>\nb\n</li>\n</ol>\n<ol start=\"5\" type=\"A\">\n<li>\nd\n</li>\n</ol>" ; "Ordered_MixedRomanAndAlpha"
    )]
    #[test_case(
        "The civil war ended in\n1865. And this should not start a list.",
        "<p>The civil war ended in\n1865. And this should not start a list.</p>" ; "NoListAmbiguity"
    )]
    fn test_lists(input: &str, output: &str) {
        let (_, md) = parse_markdown(input)
            .unwrap_or_else(|err| panic!("Parsing failed for input {:?}: {:?}", input, err));
        assert_eq!(translate(md), output);
    }
}
