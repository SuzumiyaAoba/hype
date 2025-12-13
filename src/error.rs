use anstyle::{AnsiColor, Reset, Style};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Range<usize>,
    pub source: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

/// エラーをカラフルに整形（赤アンダーカールと ^ を表示）。
pub fn format_error(err: &ParseError) -> String {
    let (line, col, line_start, line_end) = line_info(&err.source, err.span.start);
    let line_str = &err.source[line_start..line_end];
    let caret_pad = " ".repeat(col.saturating_sub(1));

    let red_bold = Style::new().fg_color(Some(AnsiColor::Red.into())).bold();
    let dim = Style::new().fg_color(Some(AnsiColor::BrightBlack.into()));
    let reset = Reset.render();
    const UNDERCURL_RED: &str = "\u{001b}[31;4:3m";

    let under = underline_slice(line_str, col, err.span.len().max(1));

    format!(
        "{hdr} error{reset}\n{dim}│{reset}  at line {line}, col {col}\n{dim}│{reset}  {pre}{curl}{target}{reset}{post}\n{dim}└{reset}  {pad}{caret}\n   {pad}{msg}",
        hdr = red_bold.render(),
        line = line,
        col = col,
        pre = under.pre,
        target = under.target,
        post = under.post,
        curl = UNDERCURL_RED,
        dim = dim.render(),
        reset = reset,
        pad = caret_pad,
        caret = format!("{}^{}", red_bold.render(), reset),
        msg = err.message
    )
}

struct Underline<'a> {
    pre: &'a str,
    target: &'a str,
    post: &'a str,
}

fn underline_slice<'a>(line: &'a str, col: usize, len: usize) -> Underline<'a> {
    let mut start_byte = line.len();
    let mut end_byte = line.len();
    let mut current_col = 1usize;

    for (i, _) in line.char_indices() {
        if current_col == col {
            start_byte = i;
            break;
        }
        current_col += 1;
    }

    if start_byte == line.len() && col == 1 {
        start_byte = 0;
    }

    let mut count = 0usize;
    for (i, _) in line[start_byte..].char_indices() {
        if count == len {
            end_byte = start_byte + i;
            break;
        }
        count += 1;
    }
    if count < len {
        end_byte = line.len();
    }

    let pre = &line[..start_byte.min(line.len())];
    let target = &line[start_byte.min(line.len())..end_byte.min(line.len())];
    let post = &line[end_byte.min(line.len())..];

    Underline { pre, target, post }
}

fn line_info(src: &str, offset: usize) -> (usize, usize, usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut last_line_start = 0usize;
    for (i, ch) in src.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            last_line_start = i + ch.len_utf8();
        } else {
            col += 1;
        }
    }

    let line_end = src[last_line_start..]
        .find('\n')
        .map(|idx| last_line_start + idx)
        .unwrap_or_else(|| src.len());

    (line, col, last_line_start, line_end)
}
