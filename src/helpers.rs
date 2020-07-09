//run: cargo test -- --nocapture

#[test]
fn grid_walk_test() {
}

#[derive(Debug)]
pub struct TextGridWalk<'a> {
    // This is really the only value we want to allow accessed
    pub cur_line: &'a str,
    pub row_start_index: usize,
    pub col_index: usize,

    line_source: SplitInclusive<'a>,
    char_source: std::str::Chars<'a>,
}

impl<'a> TextGridWalk<'a> {
    pub fn new(buffer: &'a str) -> Self {
        Self {
            cur_line: "",
            line_source: split_inclusive(buffer, '\n'),
            char_source: "".chars(),
            row_start_index: 0,
            col_index: 0,
        }
    }

    pub fn walk_to_line_end(&mut self) -> &str {
        self.char_source = "".chars();
        &self.cur_line[self.col_index..]
        // NOTE: we do not update self.col_index
        //self.col_index = self.cur_line.len();
    }
}

impl<'a> Iterator for TextGridWalk<'a> {
    type Item = char;
    // NOTE: 'self.col_index' is set to +1 over returned Item
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.char_source.next() {
            self.col_index += 1;
            Some(ch)
        } else {
            let cur_line_len = self.cur_line.len();
            self.cur_line = self.line_source.next()?;
            self.char_source = self.cur_line.chars();
            self.row_start_index += cur_line_len;
            self.col_index = 0;
            self.char_source.next()
        }
    }
}

/// This is only available on nightly, use for split.lines()
/// This will work for '\r\n' as well
pub fn split_inclusive(buffer: &str, pattern: char) -> SplitInclusive {
    SplitInclusive(buffer, pattern)
}

#[derive(Debug)]
pub struct SplitInclusive<'a>(&'a str, char);

impl<'a> Iterator for SplitInclusive<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let Self(rest, pattern) = self;
        rest.find(*pattern)
            .or_else(|| {
                if rest.is_empty() {
                    None
                } else {
                    Some(rest.len() - 1)
                }
            })
            .map(|i| {
                let (to_return, to_process) = rest.split_at(i + 1);
                *rest = to_process;
                to_return
            })
    }
}

#[test]
fn splitting_inclusively() {
    let text = "foo\r\nbar\n\nbaz\n";
    let mut lines = split_inclusive(text, '\n');
    assert_eq!(Some("foo\r\n"), lines.next());
    assert_eq!(Some("bar\n"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("baz\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "\n\n";
    let mut lines = split_inclusive(text, '\n');
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "\n";
    let mut lines = split_inclusive(text, '\n');
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "";
    assert_eq!(None, split_inclusive(text, '\n').next());

    let text = "hello";
    let mut lines = split_inclusive(text, '\n');
    assert_eq!(Some("hello"), lines.next());
    assert_eq!(None, lines.next());

    let text = "hello\ndarkness";
    let mut lines = split_inclusive(text, '\n');
    assert_eq!(Some("hello\n"), lines.next());
    assert_eq!(Some("darkness"), lines.next());
    assert_eq!(None, lines.next());
}

// @CLEANUP this is in nightly as then_some()
pub trait OwnedToOption<T> {
    fn to_some(self, val: T) -> Option<T>;
}

impl<T> OwnedToOption<T> for bool {
    fn to_some(self, val: T) -> Option<T> {
        if self {
            Some(val)
        } else {
            None
        }
    }
}
